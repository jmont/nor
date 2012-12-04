module Test where
import Patch
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Control.Applicative
import Control.Monad
import Data.List
import Nor

instance (Eq t, Arbitrary t) => Arbitrary (Edit t) where
    arbitrary = oneof [return C,
                       liftM D arbitrary,
                       liftM I arbitrary]

instance Arbitrary ChangeHunk where
    arbitrary = do
        NonNegative n <- arbitrary `suchThat` (<50) -- offset
        olds <- arbitrary `suchThat` ((<20) . length) -- lines changed
        news <- arbitrary `suchThat` ((<20) . length)
        return $ ChangeHunk n olds news

instance Arbitrary PatchAction where
    arbitrary = oneof [return CreateEmptyFile,
                       return RemoveEmptyFile,
                       liftM Change arbitrary]

instance Arbitrary t => Arbitrary (AtPath t) where
    arbitrary = do
        NonEmpty s <- arbitrary
        AP <$> return s <*> arbitrary

instance (Conflictable t, Arbitrary t) => Arbitrary (Conflict t) where
    arbitrary = do
        x <- arbitrary
        Conflict <$> return x <*> arbitrary `suchThat` (conflicts x)

instance Arbitrary File where
    arbitrary = do
        NonNegative len <- arbitrary
        conts <- arbitrary
        NonEmpty fpath <- arbitrary
        return $ File fpath (take len conts)

-- Generates several patches given a file that don't conflict
mkGoodPatch :: File -> Gen [Patch]
mkGoodPatch f = do
    frequency
        [ (1, return ( map (AP (path f)) [Change (ChangeHunk 0 (contents f) []),
                                 RemoveEmptyFile])),
          (9, do
            chs <- (mkGoodCH 0 f)
            return $ map ((AP (path f)) . Change) chs) ]

-- Generates several random change hunks given a file that don't conflict
mkGoodCH :: Int -> File -> Gen [ChangeHunk]
mkGoodCH startoff f = do
    if (startoff >= length (contents f) - 1)
    then return []
    else do off <- choose (startoff, length . contents $ f)
            endDellOff <- choose (off, (length (contents f)))
            let dels = slice off endDellOff (contents f)
            news <- arbitrary
            liftM ((ChangeHunk off dels news) :) $ mkGoodCH (endDellOff + 1) f
    where slice  :: Int -> Int -> [a] -> [a]
          slice from to xs = take (to - from + 1) (drop from xs)

-- forall x,y in ChangeHunk list, x does not conflict with y
noConflicts :: [ChangeHunk] -> Bool
noConflicts chs =
   foldr (\ch acc -> not (any (conflicts ch) chs) && acc) True chs

-- A conflicting set of change hunks (Conflict ch1s ch2s) obeys the following:
-- no conflicts within ch1s or within ch2s
-- forall x in ch1s there exits y in ch2s where x conflicts with y
-- forall y in ch2s there exits x in ch1s where y conflicts with x
isConflictSet :: Conflict [ChangeHunk] -> Bool
isConflictSet (Conflict ch1s ch2s) =
   noConflicts ch1s && noConflicts ch2s &&
   foldr (\ch acc -> any (conflicts ch) ch1s && acc) True ch2s &&
   foldr (\ch acc -> any (conflicts ch) ch2s && acc) True ch1s

-- Ensure the list of non-conflicting ChangeHunks from getChangeHConfs does
-- not contain any conflicting CHs
prop_noConfs :: File -> Gen Property
prop_noConfs f = do
   ch1s <- mkGoodCH 0 f
   ch2s <- mkGoodCH 0 f
   let (noConfs,_) = getChangeHConfs ch1s ch2s
   return $ classify (null noConfs) "Everything conflicts" (noConflicts noConfs)

-- forall x, exists y s.t. x conflicts y. x,y in a given conflict set
prop_eachHasConflict :: File -> Gen Property
prop_eachHasConflict f = do
   ch1s <- mkGoodCH 0 f
   ch2s <- mkGoodCH 0 f
   let (_,confLists) = getChangeHConfs ch1s ch2s
   let b = foldr (\conf acc -> isConflictSet conf && acc) True confLists
   return $ classify (null confLists) "Nothing conflicts" b

--Ugly property! Should some of this be in the  type class conflictable?
--Also 50-70% are fairly "easy" cases

-- Forall x in a conflict, forall y not in the conflict, x does not
-- conflict with y
prop_maximalConflictSet :: File -> Gen Property
prop_maximalConflictSet f = do
   ch1s <- mkGoodCH 0 f
   ch2s <- mkGoodCH 0 f
   let (noConfs,confLists) = getChangeHConfs ch1s ch2s
   let allMaximalConflicts = foldr (\conf acc ->
        let restConf = filter (/= conf) confLists
            fstInd = all (chIndependentOfConfs restConf) (firstConf conf)
            sndInd = all (chIndependentOfConfs restConf) (secondConf conf)
            noConfsInd = all (chIndependentOf noConfs)
                           (firstConf conf ++ secondConf conf)
            isMaximalConflict = fstInd && sndInd && noConfsInd
        in isMaximalConflict && acc) True confLists
   return $ classify (null confLists || null noConfs)
      "Either empty conflict list or empty non-conflict list"
      allMaximalConflicts
   --Independent meaning the ch doesn't conflict with any in the conflict set
   where chIndependentOf :: [ChangeHunk] -> ChangeHunk -> Bool
         chIndependentOf ch1s ch =
            noConflicts (ch:ch1s)
         chIndependentOfConf :: Conflict [ChangeHunk] -> ChangeHunk -> Bool
         chIndependentOfConf (Conflict ch1s ch2s) ch =
            chIndependentOf ch1s ch && chIndependentOf ch2s ch
         chIndependentOfConfs :: [Conflict [ChangeHunk]] -> ChangeHunk -> Bool
         chIndependentOfConfs confs ch =
            all (\conf -> chIndependentOfConf conf ch) confs

-- Tests our mkGoodCh to ensure no conflicts on same file
prop_mkGoodCh :: File -> Gen Bool
prop_mkGoodCh f = mkGoodCH 0 f >>= return . noConflicts

-- Applying . getEdits is the identity function
prop_getApplyEdits :: (Eq t, Arbitrary t, Show t) => [t] -> [t] -> Bool
prop_getApplyEdits x y =
    let es = getEdits x y
    in applyEdits es x == y

-- Ensures isomoprhism between canonical edits and changeHunks
prop_changeHunkEditIso :: [String] -> [String] -> Property
prop_changeHunkEditIso x y =
        let es = getEdits x y
            chs = editsToChangeHunks es
        in classify ((null x) ||  (null y)) "Either empty"
            (changeHunksToEdits chs (length x) 0 == es)

--sPP [1,2,3] == sPP [any permutation of 1,2,3]
prop_parallelPatchSequencing :: ParallelPatches -> Bool
prop_parallelPatchSequencing ps =
    let onlyCHs = take 5 $ filter (\(AP _ x) -> (x /= RemoveEmptyFile) && (x /= CreateEmptyFile)) ps
        patchesP = map sequenceParallelPatches (permutations onlyCHs)
    in foldr (\p acc -> p == (head patchesP) && acc)
        True (tail patchesP)

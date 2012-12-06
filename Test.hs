{-# LANGUAGE TemplateHaskell #-}
module Test where
import Patch
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.All
import Control.Applicative
import Control.Monad
import Data.List
import Nor
import Cases

-- Run all prop_* functions in quickCheck.
runTests = $quickCheckAll

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

------------------------------------------------------------------------------
--Utility Functions
------------------------------------------------------------------------------

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

-- Take a slice (a la python) from a list
slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

-- forall x,y in a list of conflictable types, x does not conflict with y
noConflicts :: Conflictable t => [t] -> Bool
noConflicts chs =
   foldr (\ch acc -> not (any (conflicts ch) chs) && acc) True chs

-- A conflicting set of change hunks (Conflict ch1s ch2s) obeys the following:
-- no conflicts within ch1s or within ch2s
-- forall x in ch1s there exits y in ch2s where x conflicts with y
-- forall y in ch2s there exits x in ch1s where y conflicts with x
isConflictSet :: Conflictable t => Conflict [t] -> Bool
isConflictSet (Conflict t1s t2s) =
   noConflicts t1s && noConflicts t2s &&
   foldr (\t acc -> any (conflicts t) t1s && acc) True t2s &&
   foldr (\t acc -> any (conflicts t) t2s && acc) True t1s

-- Helper function to generate non-conflicting and conflicting changehunks
-- from 3 states of a file: lca, va, vb.
generateAndMergeCHs :: [String] -> [String] -> [String] ->
    ([ChangeHunk], [Conflict [ChangeHunk]])
generateAndMergeCHs c0 c1 c2 =
    let c01 = editsToChangeHunks (getEdits c0 c1)
        c02 = editsToChangeHunks (getEdits c0 c2)
        (noConfs, confs) = getChangeHConfs c01 c02
    in (noConfs, confs)

------------------------------------------------------------------------------
-- Properties
------------------------------------------------------------------------------

-- Ensure the list of non-conflicting ChangeHunks from getChangeHConfs does
-- not contain any conflicting CHs
prop_noConfs :: File -> Property
prop_noConfs f = do
   ch1s <- mkGoodCH 0 f
   ch2s <- mkGoodCH 0 f
   let (noConfs,_) = getChangeHConfs ch1s ch2s
   classify (null noConfs) "Everything conflicts" (noConflicts noConfs)

-- forall x, exists y s.t. x conflicts y. x,y in a given conflict set
prop_eachHasConflict :: File -> Property
prop_eachHasConflict f = do
   ch1s <- mkGoodCH 0 f
   ch2s <- mkGoodCH 0 f
   let (_,confLists) = getChangeHConfs ch1s ch2s
   let b = foldr (\conf acc -> isConflictSet conf && acc) True confLists
   classify (null confLists) "Nothing conflicts" b

--Ugly property! Should some of this be in the  type class conflictable?
--Also 50-70% are fairly "easy" cases

-- Forall x in a conflict, forall y not in the conflict, x does not
-- conflict with y
prop_maximalConflictSet :: File -> Property
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
   classify (null confLists || null noConfs)
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
        in classify ((null x) || (null y)) "Either empty"
            (changeHunksToEdits chs (length x) 0 == es)

-- Olds from getConflictOlds is a subset of the original file
-- TODO can be made a stricter test by checking offsets
prop_getConflictOlds :: [String] -> [String] -> [String] -> Bool
prop_getConflictOlds c0 c1 c2 =
    let (_, confCHs) = generateAndMergeCHs c0 c1 c2
    in all (\olds -> isInfixOf olds (contents (File "foo" c0)))
                     (map getConflictOlds confCHs)

prop_getConflictOlds' :: File -> Gen Bool
prop_getConflictOlds' f = do
   ch1s <- mkGoodCH 0 f
   ch2s <- mkGoodCH 0 f
   let (_,confLists) = getChangeHConfs ch1s ch2s
   return $ all (\olds -> isInfixOf olds (contents f))
                           (map getConflictOlds confLists)

-- Tests that conflictAsCH (creating a viewable conflict) doesn't introduce
-- more conflicts
prop_viewableConflict :: [String] -> [String] -> [String] -> Property
prop_viewableConflict c0 c1 c2 =
    let (noConfs, confs) = generateAndMergeCHs c0 c1 c2
        viewableConflicts = map conflictAsCH confs
    in classify (null confs) "Empty non-conflict list"
            $ noConflicts (viewableConflicts ++ noConfs)

prop_viewableConflict' :: File -> Gen Property
prop_viewableConflict' f = do
   ch1s <- mkGoodCH 0 f
   ch2s <- mkGoodCH 0 f
   let (noConfs,confLists) = getChangeHConfs ch1s ch2s
   let viewableConflicts = map conflictAsCH confLists
   return $ classify (null confLists) "Empty non-conflict list"
      $ noConflicts (viewableConflicts ++ noConfs)

--sPP [1,2,3] == sPP [any permutation of 1,2,3]
prop_parallelPatchSequencing :: ParallelPatches -> Bool
prop_parallelPatchSequencing ps =
    let onlyCHs = take 5 $ filter (\(AP _ x) -> (x /= RemoveEmptyFile) && (x /= CreateEmptyFile)) ps
        patchesP = map sequenceParallelPatches (permutations onlyCHs)
    in foldr (\p acc -> p == (head patchesP) && acc)
        True (tail patchesP)

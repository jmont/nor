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
import qualified Data.Set as Set

data PPatchesFromFiles = PPF ParallelPatches ParallelPatches
    deriving (Show)
instance Arbitrary PPatchesFromFiles where
    arbitrary = do
        f1 <- arbitrary
        f2 <- arbitrary `suchThat` (\f2 -> path f2 /= path f1)
        ppa1 <- mkGoodPPatch f1
        ppb1 <- mkGoodPPatch f1
        ppa2 <- mkGoodPPatch f2
        ppb2 <- mkGoodPPatch f2
        return $ PPF (ppa1 ++ ppa2) (ppb1 ++ ppb2)
    shrink (PPF [] []) = []
    shrink (PPF p1s (p2:p2s)) = PPF p1s p2s : shrink (PPF p1s p2s)
    shrink (PPF (p1:p1s) p2s) = PPF p1s p2s : shrink (PPF p2s p1s)

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
                       liftM RemoveFile arbitrary,
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
mkGoodPPatch :: File -> Gen [Patch]
mkGoodPPatch f = do
    frequency
        [ (1, return ( [AP (path f) (RemoveFile (contents f))])),
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

isIndependentOfList :: Conflictable t => t -> [t] -> Bool
isIndependentOfList t ts = not $ any (conflicts t) ts

isIndependentOfConf :: Conflictable t => t -> Conflict [t] -> Bool
isIndependentOfConf t (Conflict t1s t2s) =
   isIndependentOfList t t1s && isIndependentOfList t t2s

isMaximalConflicts :: (Eq t,Conflictable t) => [t] -> [Conflict [t]] -> Bool
isMaximalConflicts noConfs confLists = all (\conf ->
      let restConf = filter (/= conf) confLists
          fstInd = all (flip isIndependentOfConfs restConf) (firstConf conf)
          sndInd = all (flip isIndependentOfConfs restConf) (secondConf conf)
          noConfsInd = all (flip isIndependentOfList noConfs)
                           (firstConf conf ++ secondConf conf)
      in fstInd && sndInd && noConfsInd) confLists
   where isIndependentOfConfs :: Conflictable t => t -> [Conflict [t]] -> Bool
         isIndependentOfConfs t confs =
            all (isIndependentOfConf t) confs

-- Helper function to generate non-conflicting and conflicting patches
-- from 3 states of a file: lca, va, vb.
generateAndMergePatches :: [String] -> [String] -> [String] ->
    (ParallelPatches, [Conflict ParallelPatches])
generateAndMergePatches c0 c1 c2 =
    let p01 = editsToPatch (getEdits c0 c1) "test"
        p02 = editsToPatch (getEdits c0 c2) "test"
        (noConfs, confs) = p01 >||< p02
    in (noConfs, confs)

generateAndMergePatches' :: [String] -> [String] -> [String] ->
    (ParallelPatches, [Conflict ParallelPatches])
generateAndMergePatches' c0 c1 c2 =
    let p01 = editsToPatch (getEdits c0 c1) "test"
        p02 = editsToPatch (getEdits c0 c2) "test"
        (noConfs, confs) = mergeParallelPatches p01 p02
    in (noConfs, confs)
------------------------------------------------------------------------------
-- Properties
------------------------------------------------------------------------------

-- Ensure the list of non-conflicting ChangeHunks from getChangeHConfs does
-- not contain any conflicting CHs
prop_noConfs :: PPatchesFromFiles -> Property
prop_noConfs (PPF p1s p2s) =
   let (noConfs,_) = mergeParallelPatches p1s p2s
   in classify (null noConfs) "Everything conflicts" (noConflicts noConfs)

-- forall x, exists y s.t. x conflicts y. x,y in a given conflict set
prop_eachHasConflict :: PPatchesFromFiles -> Property
prop_eachHasConflict (PPF p1s p2s) =
   let (_,confLists) = mergeParallelPatches p1s p2s
       b = foldr (\conf acc -> isConflictSet conf && acc) True confLists
   in classify (null confLists) "Nothing conflicts" b

-- Forall x in a conflict, forall y not in the conflict, x does not
-- conflict with y
prop_maximalConflictSet :: PPatchesFromFiles -> Property
prop_maximalConflictSet (PPF p1s p2s) =
   let (noConfs,confLists) = mergeParallelPatches p1s p2s
   in classify (null confLists || null noConfs)
      "Either empty conflict list or empty non-conflict list"
      (isMaximalConflicts noConfs confLists)

-- Tests our mkGoodCH to ensure no conflicts on same file
prop_mkGoodCH :: File -> Gen Bool
prop_mkGoodCH f = liftM noConflicts $ mkGoodCH 0 f

prop_mkGoodPPatch :: PPatchesFromFiles -> Bool
prop_mkGoodPPatch (PPF p1s p2s) = noConflicts p1s && noConflicts p2s

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
    let (_, confPs) = generateAndMergePatches c0 c1 c2
        confCHs = map (\(Conflict a b) ->
                            Conflict (filter isCH a) (filter isCH b)) confPs
        -- Have: [Conflict ParallelPatches]
        -- Want: [AtPath Conflict [ChangeHunk]]
        confCHs' = map (\(Conflict appas1 appas2) ->
                            let p = appath (head appas1)
                                chs1 =  map (\(AP _ pa) -> fromChange pa) appas1
                                chs2 =  map (\(AP _ pa) -> fromChange pa) appas2
                                in AP p (Conflict chs1 chs2)) confCHs

    in all (\olds -> isInfixOf olds (contents (File "foo" c0)))
                     (map (getConflictOlds . (\(AP _ x) -> x)) confCHs')

-- Tests that conflictAsCH (creating a viewable conflict) doesn't introduce
-- more conflicts
prop_viewableConflict' :: PPatchesFromFiles -> Property
prop_viewableConflict' (PPF p1s p2s) =
    let (noConfs,confs) = p1s `mergeParallelPatches` p2s
    -- confs :: [Conflict ParallelPatches]
    -- confs :: [Conflict [Patches]]
    -- confs :: [Conflict [AtPath PatchAction]]
        foo =   concatMap (\(Conflict appas1 appas2) ->
                       let gpa1s = groupByPath appas1
                       -- gpa1s :: [AtPath [PatchAction]] from state1
                           gpa2s = groupByPath appas2
                       -- gpa2s :: [AtPath [PatchAction]] from state2
                           bar = groupByPath (gpa1s ++ gpa2s)
                       -- one [PatchAction] at each path per state
                       -- bar :: [AtPath [[PatchAction]]] with each path
                       -- having a list of two elements
                       in bar)
               confs --
        -- foo :: [AtPath [[PatchAction]]]
        viewableConflicts = map (\(AP p [pa1s,pa2s]) ->
                        conflictAsPatch' (AP p (Conflict pa1s pa2s)))
               foo
        -- viewableConflicts :: [Patch]
    in classify (null confs) "Empty non-conflict list"
        $ noConflicts (viewableConflicts ++ noConfs)

-- Tests that conflictAsCH (creating a viewable conflict) doesn't introduce
-- more conflicts
prop_viewableConflict :: PPatchesFromFiles -> Property
prop_viewableConflict (PPF p1s p2s) =
   let (noConfs,confs) = p1s >||< p2s
       viewableConflicts = map conflictAsPatch confs
   in classify (null confs) "Empty non-conflict list"
      $ noConflicts (viewableConflicts ++ noConfs)

--sPP [1,2,3] == sPP [any permutation of 1,2,3]
prop_parallelPatchSequencing :: ParallelPatches -> Bool
prop_parallelPatchSequencing ps =
    let onlyCHs = take 5 $ filter isCH ps
        patchesP = map sequenceParallelPatches (permutations onlyCHs)
    in foldr (\p acc -> p == (head patchesP) && acc)
        True (tail patchesP)

prop_ungroupByPath :: [AtPath [Int]] -> Property
prop_ungroupByPath aps = all (not . null . fromPath) aps ==>
    aps == groupByPath (ungroupByPath aps)

prop_groupByPath :: [AtPath Int] -> Bool
prop_groupByPath aps = aps == ungroupByPath (groupByPath aps)

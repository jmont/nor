{-# LANGUAGE TemplateHaskell #-}
module Test where
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.All
import Control.Applicative
import Control.Monad
import Data.List
import qualified Data.Char as Char
import Data.Maybe
import qualified Data.Set as Set
import qualified Control.Monad.State as S

import Nor
import Main
import qualified ObjectStore as O
import Core
import World
import Patch

data BranchedCore = BC Core (Commit O.Hash) (Commit O.Hash)
    deriving (Show)
instance Arbitrary BranchedCore where
  arbitrary = do
    f <- arbitrary `suchThat` ((>7) . length . contents)
    ps <- mkGoodPPatches f `suchThat` ((>2) . length)
    split <- choose (1, length ps - 1)
    let (br1p, br2p) = splitAt split (sort ps)
    let br1s = sequenceParallelPatches br1p
    let br2s = sequenceParallelPatches br2p
    let core@(cs, os) = initCore
    let (hc, core') = S.runState (addCommit [f] (cid (Set.findMin cs))) core
    let (branch1, core'') = foldl applyPatchToCore (hc, core') br1s
    let (branch2, core''') = foldl applyPatchToCore (hc, core'') br2s
    return $ BC core''' branch1 branch2
    where applyPatchToCore :: (Commit O.Hash,Core) -> SequentialPatch ->
                              (Commit O.Hash,Core)
          applyPatchToCore (hc, core@(_,os)) p =
              let Just f = mapM (O.getObject os) (cContents hc)
              in S.runState (addCommit f (cid hc)) core
--  shrink (BC (comSet,os) b1 b2)
--   | parent b1 == parent b2 = []
--   | otherwise              =
--      (BC (comSet,os) b1 (parent b2)) : shrink (BC (comSet,os) (parent b2) b1)

data PPatchesFromFiles = PPF ParallelPatches ParallelPatches
    deriving (Show)
instance Arbitrary PPatchesFromFiles where
    arbitrary = do
        f1 <- arbitrary
        f2 <- arbitrary `suchThat` (\f2 -> path f2 /= path f1)
        ppa1 <- mkGoodPPatches f1
        ppb1 <- mkGoodPPatches f1
        ppa2 <- mkGoodPPatches f2
        ppb2 <- mkGoodPPatches f2
        f3 <- arbitrary `suchThat` (\f3 -> (path f1 /= path f3) &&
                                           (path f2 /= path f3))
        f4 <- arbitrary `suchThat` (\f4 -> (path f1 /= path f4) &&
                                           (path f2 /= path f4))
        return $ PPF (AP (path f3) (CreateFile (contents f3)) : ppa1 ++ ppa2)
                     (AP (path f4) (CreateFile (contents f4)) : ppb1 ++ ppb2)
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
    arbitrary = oneof [liftM CreateFile arbitrary,
                       liftM RemoveFile arbitrary,
                       liftM Change arbitrary]

instance Arbitrary t => Arbitrary (AtPath t) where
    arbitrary = do
        NonEmpty s <- arbitrary
        AP <$> return s <*> arbitrary

instance (Conflictable t, Arbitrary t) => Arbitrary (Conflict t) where
    arbitrary = do
        x <- arbitrary
        Conflict <$> return x <*> arbitrary `suchThat` conflicts x


goodChars :: String
goodChars = ['a'..'z'] ++ ['A'..'Z']

data AsciiChar = AsciiChar Char
instance Arbitrary AsciiChar where
    --arbitrary = arbitrary `suchThat` Char.isAscii >>= return . AsciiChar
    arbitrary = elements goodChars >>= return . AsciiChar
type AsciiStr = [AsciiChar]

asciiChartoChar :: AsciiChar -> Char
asciiChartoChar (AsciiChar ch) = ch

asciiStrToString :: AsciiStr -> String
asciiStrToString aStr = map asciiChartoChar aStr

instance Arbitrary File where
    arbitrary = do
        NonNegative len <- arbitrary
        conts <- arbitrary
        NonEmpty fpath <- arbitrary
        return $ File (asciiStrToString fpath) (take len (map asciiStrToString conts))

------------------------------------------------------------------------------
--Utility Functions
------------------------------------------------------------------------------

-- Generates several patches given a file that don't conflict
mkGoodPPatches :: File -> Gen ParallelPatches
mkGoodPPatches f =
    frequency
        [ (1, return [AP (path f) (RemoveFile (contents f))]),
          (9, do
            chs <- mkGoodCHs 0 f
            return $ map (AP (path f) . Change) chs) ]


-- Generates several random change hunks given a file that don't conflict
mkGoodCHs :: Int -> File -> Gen [ChangeHunk]
mkGoodCHs startoff f =
    if startoff >= length (contents f) - 1
    then return []
    else do off <- choose (startoff, length (contents f))
            endDellOff <- choose (off, length (contents f))
            let dels = slice off endDellOff (contents f)
            news <- arbitrary
            liftM (ChangeHunk off dels (map asciiStrToString news) :)
                  $ mkGoodCHs (endDellOff + 1) f
   where slice :: Int -> Int -> [a] -> [a]
         slice from to xs = take (to - from + 1) (drop from xs)
-- Take a slice (a la python) from a list

-- forall x,y in a list of conflictable types, x does not conflict with y
noConflicts :: Conflictable t => [t] -> Bool
noConflicts chs =
   noDistinctPairs conflicts chs

-- A conflicting set of change hunks (Conflict ch1s ch2s) obeys the following:
-- no conflicts within ch1s or within ch2s
-- forall x in ch1s there exits y in ch2s where x conflicts with y
-- forall y in ch2s there exits x in ch1s where y conflicts with x
isConflictSet :: Conflictable t => Conflict [t] -> Bool
isConflictSet (Conflict t1s t2s) =
   noConflicts t1s && noConflicts t2s &&
   all (\t -> any (conflicts t) t1s) t2s &&
   all (\t -> any (conflicts t) t2s) t1s

isIndependentOfList :: Conflictable t => t -> [t] -> Bool
isIndependentOfList t ts = not $ any (conflicts t) ts

isIndependentOfConf :: Conflictable t => t -> Conflict [t] -> Bool
isIndependentOfConf t (Conflict t1s t2s) =
   isIndependentOfList t t1s && isIndependentOfList t t2s

isMaximalConflicts :: (Eq t,Conflictable t) => [t] -> [Conflict [t]] -> Bool
isMaximalConflicts noConfs confLists = all (\conf ->
      let restConf = filter (/= conf) confLists
          fstInd = all (`isIndependentOfConfs` restConf) (firstConf conf)
          sndInd = all (`isIndependentOfConfs` restConf) (secondConf conf)
          noConfsInd = all (`isIndependentOfList` noConfs)
                           (firstConf conf ++ secondConf conf)
      in fstInd && sndInd && noConfsInd) confLists
   where isIndependentOfConfs :: Conflictable t => t -> [Conflict [t]] -> Bool
         isIndependentOfConfs t = all (isIndependentOfConf t)

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
       b = all isConflictSet confLists
   in classify (null confLists) "Nothing conflicts" b

-- Forall x in a conflict, forall y not in the conflict, x does not
-- conflict with y
prop_maximalConflictSet :: PPatchesFromFiles -> Property
prop_maximalConflictSet (PPF p1s p2s) =
   let (noConfs,confLists) = mergeParallelPatches p1s p2s
   in classify (null confLists || null noConfs)
      "Either empty conflict list or empty non-conflict list"
      (isMaximalConflicts noConfs confLists)

-- Tests our mkGoodCHs to ensure no conflicts on same file
prop_mkGoodCHs :: File -> Gen Bool
prop_mkGoodCHs f = liftM noConflicts $ mkGoodCHs 0 f

prop_mkGoodPPatches :: PPatchesFromFiles -> Bool
prop_mkGoodPPatches (PPF p1s p2s) = noConflicts p1s && noConflicts p2s

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
        in classify (null x || null y) "Either empty"
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

    in all (\olds -> olds `isInfixOf` contents (File "foo" c0))
                     (map (getConflictOlds . (\(AP _ x) -> x)) confCHs')

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
    in all (== head patchesP) (tail patchesP)

-- For a symmetric relation p, returns True if the relation is not present
-- between any two elements of the list
noDistinctPairs :: (a -> a -> Bool) -> [a] -> Bool
noDistinctPairs p [] = True
noDistinctPairs p (a:as) = not (any (p a) as) && noDistinctPairs p as

chooseLeft :: [Conflict ParallelPatches] -> ResolvedConflicts
chooseLeft = concatMap (\(Conflict p1s _) -> p1s)

chooseRight :: [Conflict ParallelPatches] -> ResolvedConflicts
chooseRight = concatMap (\(Conflict _ p2s) -> p2s)

identicalConf :: Conflict ParallelPatches -> Bool
identicalConf (Conflict p1 p2) = p1 == p2

resolveIdenticals :: RebaseRes -> RebaseRes
resolveIdenticals (konf@(Conf c hc confs noConfs (toR:toRs) lca)) =
  if all identicalConf confs
    then resolveIdenticals $ resolve c hc (chooseLeft confs ++ noConfs) toRs lca
    else konf
resolveIdenticals succ = succ

prop_madeTwoBranches :: BranchedCore -> Bool
prop_madeTwoBranches (BC core b1 b2) =
  let bs = getBranches core
  in bs == [b1,b2] || bs == [b2,b1]

prop_rebaseEq :: BranchedCore -> Bool
prop_rebaseEq (BC core b1 b2) =
  case (rebaseStart core b1 b2, rebaseStart core b2 b1) of
   (Succ (_,os1) hc1, Succ (_,os2) hc2) ->
      let reb1Files = fromJust $ mapM (O.getObject os1) (cContents hc1)
          reb2Files = fromJust $ mapM (O.getObject os2) (cContents hc2)
      in reb1Files == reb2Files
   _ -> False

prop_rebaseSucceeds :: BranchedCore -> Bool
prop_rebaseSucceeds (BC core b1 b2) =
  case resolveIdenticals (rebaseStart core b1 b2) of
    Succ _ _ -> True
    otherwise -> False

prop_rebaseBothSucc :: BranchedCore -> Bool
prop_rebaseBothSucc (bc@(BC core b1 b2)) =
  prop_rebaseSucceeds bc && prop_rebaseSucceeds (BC core b2 b1)

failureWorld :: (BranchedCore -> Bool) -> IO BranchedCore
failureWorld bcProp = do
  bcs <- sample' arbitrary
  let Just (BC core b1 b2) = find (not . bcProp) bcs
  return (BC core b1 b2)
  --(core, Ephemera b1 [])

testRebaseStart :: World -> RebaseRes
testRebaseStart (core@(comSet,os),_) =
  let [com1,com2] = getBranches core
  in rebaseStart core com1 com2

getBranches :: Core -> [Commit O.Hash]
getBranches (comSet,_) =
  let parentList = catMaybes $ Set.toList $ Set.map parent comSet
      hashes = Set.toList $ Set.map cid comSet
      branches = hashes \\ parentList
  in map (getCommit comSet) branches
  where getCommit :: Set.Set (Commit O.Hash) -> O.Hash -> Commit O.Hash
        getCommit comSet h = head $ Set.toList $ Set.filter ((h==).cid) comSet

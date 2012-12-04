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

mkGoodPatch :: File -> Gen [Patch]
mkGoodPatch f = do
    frequency
        [ (1, return ( map (AP (path f)) [Change (ChangeHunk 0 (contents f) []),
                                 RemoveEmptyFile])),
          (9, do
            chs <- (mkGoodCH 0 f)
            return $ map ((AP (path f)) . Change) chs) ]

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

noConflicts :: [ChangeHunk] -> Bool
noConflicts chs =
   foldr (\ch acc -> not (any (conflicts ch) chs) && acc) True chs

prop_mkGoodCh :: File -> Gen Bool
prop_mkGoodCh f = mkGoodCH 0 f >>= return . noConflicts

prop_getApplyEdits :: (Eq t, Arbitrary t, Show t) => [t] -> [t] -> Bool
prop_getApplyEdits x y =
    let es = getEdits x y
    in applyEdits es x == y

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

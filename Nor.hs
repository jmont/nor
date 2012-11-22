module Nor where
import Control.Monad
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Data.List
import Diff
import Data.Algorithm.Diff
import Patch
import qualified Data.Set as Set
import ObjectStore
import Crypto.Hash.SHA1 (hashlazy, hash)
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString as Strict
import Data.Serialize
import qualified Control.Monad.State as S
---------------------------------

data File = File { path :: String -- Unix filepath: "/foo/bar/baz"
                 , contents :: [String] -- Simple representation for now
                 } deriving (Show)

instance Serialize File where
    put f = put (path f,contents f)
    get = getTwoOf (get :: Get String) (get :: Get [String]) >>=
         (\(p,cont) -> return $ File p cont)

type HashEntry = (Hash, File)
type HashDict = Map.Map Hash File

--Mapping between Hashes -> a
type WithObjects a b = S.State (ObjectStore a) b

addHashableAs :: Serialize a => [a] -> WithObjects a Hash
addHashableAs as = foldr1 (>>) (map addHashableA as)

addHashableA :: Serialize a => a -> WithObjects a Hash
addHashableA a = do
                 os <- S.get
                 let (hash,newState) = addObject os a
                 S.put newState
                 return hash

createCommit :: WithObjects File Hash -> Maybe Commit -> WithObjects File Commit
createCommit s pc =
   do
   newState <- S.get
   let (h,os) = S.runState s newState
   let hashes = getHashes os
   S.put os
   return (Commit pc hashes (hash (Strict.concat hashes)))

addCommit :: WithObjects File Commit -> Core -> Core
addCommit s (commitS, os) =
   let (newCommit,newOS) = S.runState s os
   in (Set.insert newCommit commitS,newOS)

data Commit = Commit { parent :: Maybe Commit -- Initial commit has nothing
                     , hashes :: [Hash] -- Hashes of all files at given time
                     , cid :: Hash
                     } deriving (Show)
instance Ord Commit where
   compare c1 c2 = compare (cid c1) (cid c2)
instance Eq Commit where
   (==) c1 c2 = (cid c1) == (cid c2)

-- list of all commits, hash->file, head commit, commitCount
type Core = (Set.Set Commit, ObjectStore File)
type World = (Core, Commit)

--Demo of how to use WithObjects
file1 = File "test1" ["hello"]
file2 = File "test2" ["bye"]
core = (Set.empty, mkEmptyOS)
withF1 = addHashableA file1
withF2 = addHashableA file2
withF12 = withF1 >> withF2
withF12' = addHashableAs [file1,file2]
withC = createCommit withF12 Nothing
withC' = createCommit withF12' Nothing
core'  = addCommit withC core
core'' = addCommit withC' core

-- An empty world
init :: World
init = let initC = Commit Nothing [] (hash (encode ""))
       in ((Set.singleton initC, mkEmptyOS),initC)

commitById :: Core -> Hash -> Maybe Commit
commitById (commitSet, _) id =
    foldl (\mc c@(Commit _ _ cid) ->
                if id == cid then Just c
                             else mc) Nothing (Set.elems commitSet)

medCheckout :: Core -> Commit -> Maybe [File]
medCheckout (_,os) (Commit _ hashes _) =
    sequence (map (getObject os) hashes)

getLca :: Commit -> Commit -> Commit
getLca  ca cb =
   let ancSeta = Set.fromList (ancestorList ca)
   in foldr (\a z -> if (Set.member a ancSeta) then a else z)
      (error "No LCA") (ancestorList cb)

ancestorList :: Commit -> [Commit]
ancestorList c1@(Commit Nothing _ _) = [c1]
ancestorList c1@(Commit (Just pc) _ _) = c1 : (ancestorList pc)

--Maybe not a withObjects because it doesn't create any new files?
--Return a patch from commit a to commit b
commitsToPatch :: Commit -> Commit -> WithObjects File Patch
commitsToPatch ca cb = S.state (\os ->
      let hashesA = Set.fromList (hashes ca)
          hashesB = Set.fromList (hashes cb)
          onlyA = hashesA Set.\\ hashesB
          onlyB = hashesB Set.\\ hashesA
          filesOnlyA = getFilesForSet os onlyA
          filesOnlyB = getFilesForSet os onlyB
          aPatchMap = foldr (\f pm -> Map.insert (path f)
               [ChangeHunk 0 (contents f) [], RemoveEmptyFile] pm)
               Map.empty filesOnlyA
          patchMap = foldr (\f pm -> Map.alter (alterFun f) (path f) pm)
                     aPatchMap filesOnlyB
          patch = Atomic $ Map.foldWithKey (\path pActions acc ->
                     (map (AtPath path) pActions) ++ acc) [] patchMap
          in (patch,os))
   where getFilesForSet os hashesSet =
          (fromJust (sequence (map (getObject os) (Set.toList hashesSet))))
         alterFun :: File -> Maybe [PatchAction] -> Maybe [PatchAction]
         alterFun newFile Nothing =
            Just $ [CreateEmptyFile, ChangeHunk 0 [] (contents newFile)]
         alterFun newFile (Just [(ChangeHunk _ fContents []),_]) =
            Just $ editsToChangeHunks $ getDiff fContents (contents newFile)
         alterFun _ _ = error "Can't Happen"

applyPatch :: Patch -> [File] -> [File]
applyPatch = error "Not implemented"

mergeC :: Commit -> Commit -> Commit -> Commit -> WithObjects File Commit
mergeC ca cb lca newpc = S.state (\os ->
      let wPatchA = commitsToPatch lca ca
          wPatchB = commitsToPatch lca cb
          patchAB = S.evalState (wPatchA >>= (\pa -> wPatchB >>= (\pb ->
            return $ mergePatches pa pb))) os
          lcaFiles = fromJust (sequence (map (getObject os) (hashes lca)))
          newFiles = applyPatch patchAB lcaFiles
          --What happens if some files haven't changed??
          (hs,newOS) = foldr (\f (hs,os) ->
                  let (h,os') = addObject os f
                  in (h:hs,os')) ([],os) newFiles
      in (Commit (Just newpc) hs (hash (Strict.concat hs)),newOS))

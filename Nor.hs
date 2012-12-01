module Nor where
import Control.Monad
import Control.Applicative
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
    put (File p c) = put p >> put c
    get = File <$> get <*> get

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
createCommit s pc = do
   let (h,commitOS) = S.runState s mkEmptyOS
   let hashes = getHashes commitOS
   newState <- S.get
   let (_,os) = S.runState s newState
   let pcid = (pc >>= (\x -> return (cid x)))
   S.put os
   return $ Commit pcid hashes $ mkCommitHash hashes

addCommit :: WithObjects File Commit -> Core -> World
addCommit s (commitS, os) =
   let (newCommit,newOS) = S.runState s os
   in ((Set.insert newCommit commitS, newOS), newCommit)

commit :: World -> [File] -> World
commit w@(core, head) fs =
    let fhs = addHashableAs fs
        -- ERROR - NEED TO REMOVE HASHES WITH SAME PATHS!!!
        fc = createCommit fhs (Just head)
    in addCommit fc core

mkCommitHash :: [Hash] -> Hash
mkCommitHash = Hash . hash . Strict.concat . (map getHash)

data Commit = Commit { parent :: Maybe Hash -- Initial commit has nothing
                     , hashes :: [Hash] -- Hashes of all files at given time
                     , cid :: Hash
                     } deriving (Show)
instance Ord Commit where
   compare c1 c2 = compare (cid c1) (cid c2)
instance Eq Commit where
   (==) c1 c2 = (cid c1) == (cid c2)
instance Serialize Commit where
    put (Commit pid hs id) = put pid >> put hs >> put id
    get = Commit <$> get <*> get <*> get

-- list of all commits, hash->file, head commit, commitCount
type Core = (Set.Set Commit, ObjectStore File)
type World = (Core, Commit)

instance (Serialize a) => Serialize (ObjectStore a) where
    put (OS s) = put s
    get = OS <$> get

-- An empty world
init :: World
init = let initC = Commit Nothing [] $ Hash (hash (encode ""))
       in ((Set.singleton initC, mkEmptyOS),initC)

commitById :: Core -> Hash -> Maybe Commit
commitById (commitSet, _) id =
    foldl (\mc c@(Commit _ _ cid) ->
                if id == cid then Just c
                             else mc) Nothing (Set.elems commitSet)

medCheckout :: Core -> Commit -> Maybe [File]
medCheckout (_,os) (Commit _ hashes _) =
    sequence (map (getObject os) hashes)

getLca :: Core -> Commit -> Commit -> Commit
getLca core ca cb =
   let ancSeta = Set.fromList (ancestorList core ca)
   in foldr (\a z -> if (Set.member a ancSeta) then a else z)
      (error "No LCA") (ancestorList core cb)

ancestorList :: Core -> Commit -> [Commit]
ancestorList _ c1@(Commit Nothing _ _) = [c1]
ancestorList core c1@(Commit (Just pid) _ _) =
    let Just p = commitById core pid
    in c1:(ancestorList core p)

--Return a patch from commit a to commit b
patchFromCommits :: ObjectStore File -> Commit -> Commit -> [Patch]
patchFromCommits os ca cb =
      let hashesA = Set.fromList (hashes ca)
          hashesB = Set.fromList (hashes cb)
          onlyA = hashesA Set.\\ hashesB
          onlyB = hashesB Set.\\ hashesA
          filesOnlyA = getFilesForSet os onlyA
          filesOnlyB = getFilesForSet os onlyB
          --Assume everything in A has been deleted
          aPatchMap = foldr (\f pm -> Map.insert (path f)
               [ChangeHunk 0 (contents f) [], RemoveEmptyFile] pm)
               Map.empty filesOnlyA
          --Update map, anything not found is new
          --If exists, then change to only a changehunk
          patchMap = foldr (\f pm -> Map.alter (alterFun f) (path f) pm)
                     aPatchMap filesOnlyB
          patch = Map.foldrWithKey (\path pActions acc ->
                  (map (Patch path) pActions) ++ acc) [] patchMap
          in patch
   where getFilesForSet os hashesSet =
          (fromJust (sequence (map (getObject os) (Set.toList hashesSet))))
         alterFun :: File -> Maybe [PatchAction] -> Maybe [PatchAction]
         alterFun newFile Nothing =
            Just $ [CreateEmptyFile, ChangeHunk 0 [] (contents newFile)]
         alterFun changedFile (Just [(ChangeHunk _ fContents []),_]) =
            Just $ editsToChangeHunks $ getDiff fContents (contents changedFile)
         alterFun _ _ = error "Can't Happen"

--Assumes SEQUENTIAL PATCH
applyPatch :: Patch -> [File] -> [File]
applyPatch (Patch ppath CreateEmptyFile) fs = File ppath [""]:fs
applyPatch (Patch _ RemoveEmptyFile) [] = [] --Maybe error?
--Should we check if empty file?
applyPatch p@(Patch ppath RemoveEmptyFile) (f:fs) =
   if ppath == path f then fs else f:applyPatch p fs
applyPatch p@(Patch ppath (ChangeHunk o dels adds)) [] = [] --error?
applyPatch p@(Patch ppath (ChangeHunk o dels adds)) (f:fs) =
   if ppath == path f
   then let preHunk = take o (contents f)
            rest = drop o (contents f)
            --Check if lines present?
            newcont = preHunk ++ adds ++ drop (length dels) rest
            in File (path f) newcont:fs
            else f:applyPatch p fs

applyPatches :: [Patch] -> [File] -> [File]
applyPatches ps fs = foldr applyPatch fs ps

applyConflict :: [Conflict] -> [File] -> [File]
applyConflict = error "not yet implemented"

mergeCommit :: ObjectStore File -> Commit -> Commit -> Commit ->
               ([Patch],[Conflict])
mergeCommit os ca cb lca =
      let patchTo = patchFromCommits os
          patchA = lca `patchTo` ca
          patchB = lca `patchTo` cb
      in patchA >||< patchB

parallelPatchesToCommit :: Commit -> [Patch] -> Maybe Hash ->
                           WithObjects File Commit
parallelPatchesToCommit lca patches mpcid = S.state (\os ->
      let lcaFiles = fromJust (sequence (map (getObject os) (hashes lca)))
          sPatches = seqParallelPatches patches
          newFiles = applyPatches sPatches lcaFiles
          (hs,newOS) = addObjects os newFiles
          commitHash = Hash $ hash $ Strict.concat (map getHash hs)
      in (Commit mpcid hs commitHash,newOS))

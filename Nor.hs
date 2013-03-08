module Nor where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Crypto.Hash.SHA1 (hash)
import qualified Data.ByteString as BS
import Control.Monad

import Core
import ObjectStore
import Patch

type ResolvedConflicts = ParallelPatches

getLca :: CoreReader m => Commit Hash -> Commit Hash -> m (Commit Hash)
getLca ca cb = do
   ancSetA <- liftM Set.fromList (ancestorList ca)
   ancB <- ancestorList cb
   return (foldr (\a z -> if Set.member a ancSetA then a else z)
      (error "No LCA") ancB)

ancestorList :: CoreReader m => Commit Hash -> m [Commit Hash]
ancestorList c1@(Commit Nothing _ _)    = return [c1]
ancestorList c1@(Commit (Just pid) _ _) = readCore >>= (\(cs,_) ->
   liftM (c1 :) (commitById' pid >>= ancestorList))

patchFromFiles :: [File] -> [File] -> ParallelPatches
patchFromFiles fas fbs =
    let --Assume everything in A has been deleted
        aPatchMap = foldr (\f pm -> Map.insert (path f)
             [RemoveFile (contents f)] pm)
             Map.empty fas
        --Update map, anything not found is new
        --If exists, then change to only a changehunk
        patchMap = foldr (\f pm -> Map.alter (alterFun f) (path f) pm)
                   aPatchMap fbs
        ps = Map.foldrWithKey (\path pActions acc ->
                map (AP path) pActions ++ acc) [] patchMap
    in ps
    where alterFun :: File -> Maybe [PatchAction] -> Maybe [PatchAction]
          alterFun newFile Nothing =
             Just [CreateFile (contents newFile)]
          alterFun changedFile (Just [RemoveFile fContents]) =
             Just $ map Change $ editsToChangeHunks $ getEdits fContents (contents changedFile)
          alterFun _ _ = error "Can't Happen"

--Return a patch from commit a to commit b
patchFromCommits :: CoreReader m => Commit Hash -> Commit Hash -> m ParallelPatches
patchFromCommits ca cb = readCore >>= (\(_,os) -> return
     (let hashesA = cContents ca
          hashesB = cContents cb
          onlyA = hashesA Set.\\ hashesB
          onlyB = hashesB Set.\\ hashesA
          filesOnlyA = getFilesForSet os onlyA
          filesOnlyB = getFilesForSet os onlyB
      in patchFromFiles filesOnlyA filesOnlyB))
   where getFilesForSet os hashesSet =
          fromJust $ mapM (getObject os) (Set.toList hashesSet)

--Assumes SEQUENTIAL PATCH
applyPatch :: SequentialPatch -> [File] -> [File]
applyPatch (SP (AP ppath (CreateFile c))) fs = File ppath c : fs
applyPatch (SP (AP ppath (RemoveFile _))) [] =
   error ("Deleting a file that doesn't exist:" ++ ppath)
applyPatch p@(SP (AP ppath (RemoveFile c))) (f:fs) =
   if ppath == path f
   then if c == contents f
        then fs
        else error ("Contents of RemoveFile didn't match file" ++ ppath)
   else f:applyPatch p fs
applyPatch (SP (AP ppath (Change (ChangeHunk _ _ _)))) [] =
   error ("ChangeHunk doesn't correspond to any file: " ++ ppath)
applyPatch p@(SP (AP ppath (Change (ChangeHunk o dels adds)))) (f:fs) =
   if ppath == path f
   then let preHunk = take o (contents f)
            rest = drop o (contents f)
            rest' = if dels == take (length dels) rest
                    then drop (length dels) rest
                    else error ("Deleting lines that don't exist: " ++ ppath)
            newcont = preHunk ++ adds ++ rest'
            in File (path f) newcont:fs
            else f:applyPatch p fs

applyPatches :: [SequentialPatch] -> [File] -> [File]
applyPatches ps fs = foldl (flip applyPatch) fs ps

mergeCommit :: CoreReader m => Commit Hash -> Commit Hash -> Commit Hash ->
               m (ParallelPatches,[Conflict ParallelPatches])
mergeCommit ca cb lca = do
     patchA <- patchFromCommits lca ca
     patchB <- patchFromCommits lca cb
     return $ patchA >||< patchB

parallelPatchesToCommit :: CoreExtender m => Commit Hash -> ParallelPatches ->
                           Commit Hash -> m (Commit Hash)
parallelPatchesToCommit lca patches pc= do
    lcaFiles <- getFilesForCom lca
    let sPatches = sequenceParallelPatches patches
    let newFiles = applyPatches sPatches lcaFiles
    addCommit' newFiles pc

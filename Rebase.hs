module Rebase where

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List

import Core
import ObjectStore
import Patch
import ObjectStore

data Outcome a = Succ | Fail a

data ConflictPatches = CP [Conflict ParallelPatches] ParallelPatches

data DataCommit a = DataCommit { dparent :: Maybe (DataCommit a)
                               , dcContents :: Set.Set a -- Associated as with commit
                               } deriving (Show,Read,Ord,Eq)

type ResolvedConflicts = ParallelPatches

-- Commit a is the new parent
mergeCommits :: DataCommit File -> DataCommit File -> Either (DataCommit File) ConflictPatches
mergeCommits ca cb =
   let lca = getLca ca cb
       patchA = patchFromCommits lca ca
       patchB = patchFromCommits lca cb
       (noConfs, confs) = patchA >||< patchB
   in if all identicalConf confs
         then
            -- Can we use fmap here?
            let lcaFiles = Set.toList $ dcContents lca
                sPatches = sequenceParallelPatches (noConfs ++ chooseLeft confs)
                newFiles = applyPatches sPatches lcaFiles
            in Left $ DataCommit (Just ca) (Set.fromList newFiles)
         else Right $ CP confs noConfs
    where chooseLeft :: [Conflict ParallelPatches] -> ResolvedConflicts
          chooseLeft = concatMap (\(Conflict p1s _) -> p1s)
          identicalConf :: Conflict ParallelPatches -> Bool
          identicalConf (Conflict p1 p2) = p1 == p2

replay :: DataCommit File -> [DataCommit File] -> (DataCommit File, Outcome (ConflictPatches, [DataCommit File]))
replay hc [] = (hc, Succ)
replay hc (toR:toRs) =
    case mergeCommits hc toR of
      Left hc' -> replay hc' toRs
      --Should we peel off like this?
      Right confPatches -> (hc,Fail (confPatches, toRs))

-- youngest to oldest
ancestorList :: DataCommit a -> [DataCommit a]
ancestorList c1@(DataCommit Nothing _)    = [c1]
ancestorList c1@(DataCommit (Just pc) _) = c1 : ancestorList pc

-- Always succeeds if in the same repo
getLca :: Ord a => DataCommit a -> DataCommit a -> DataCommit a
getLca dca dcb =
   let ancSetA = Set.fromList (ancestorList dca)
       ancB = ancestorList dcb
   in  foldr (\a z -> if Set.member a ancSetA then a else z)
                              (error "No LCA") ancB

patchFromCommits :: DataCommit File -> DataCommit File -> ParallelPatches
patchFromCommits dca dcb =
      let filesA = Set.toList $ dcContents dca
          filesB = Set.toList $ dcContents dcb
          filesOnlyA = filesA List.\\ filesB
          filesOnlyB = filesB List.\\ filesA
      in patchFromFiles filesA filesB

-- return a patch representing going from files in a to files in b
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

module Rebase where

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List

import Core
--import ObjectStore
import WorkingTree
import Patch
--import Repo
--import ObjectStore

data Outcome a = Succ | Fail a

data ConflictPatches = CP [Conflict ParallelPatches] ParallelPatches

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
            let lcaFiles = Set.toList $ dContents lca
                sPatches = sequenceParallelPatches (noConfs ++ chooseLeft confs)
                newFiles = applyPatches sPatches lcaFiles
            in Left $ DataCommit (Just ca) (Set.fromList newFiles)
         else Right $ CP confs noConfs
    where chooseLeft :: [Conflict ParallelPatches] -> ResolvedConflicts
          chooseLeft = concatMap (\(Conflict p1s _) -> p1s)
          identicalConf :: Conflict ParallelPatches -> Bool
          identicalConf (Conflict p1 p2) = p1 == p2

-- Assumes no conflicts
replay :: DataCommit File -> [DataCommit File] -> DataCommit File
replay fc [] = fc
replay fc (toR:toRs) =
  let lca = getLca fc toR
      ppatchF = patchFromCommits lca fc
      deltaStars = map getDeltaFromPC (toR:toRs)
      deltaTwidles = map (`adjustedByPPatch` ppatchF) deltaStars
  in foldl (\com delta -> patchCommit com delta) fc deltaTwidles
  where getDeltaFromPC :: DataCommit File -> ParallelPatches
        getDeltaFromPC (DataCommit Nothing _) = []
        getDeltaFromPC c@(DataCommit (Just pc) _) = patchFromCommits pc c
        patchCommit :: DataCommit File -> ParallelPatches -> DataCommit File
        patchCommit dc pp =
          let sp = sequenceParallelPatches pp
              newFiles = applyPatches sp (Set.toList $ dContents dc)
          in DataCommit (Just dc) (Set.fromList newFiles)


--replay :: DataCommit File -> [DataCommit File] -> (DataCommit File, Outcome (ConflictPatches, [DataCommit File]))
--replay hc [] = (hc, Succ)
--replay hc (toR:toRs) =
--    case mergeCommits hc toR of
--      Left hc' -> replay hc' toRs
--      --Should we peel off like this?
--      Right confPatches -> (hc,Fail (confPatches, toRs))

--startRebase :: WorkingTreeWriter m => HashCommit -> m (Outcome ())
--startRebase hFoundation = do
--    hfromC <- getHC
--    dfromC <- dataCommitById $ cid hfromC
--    dFoundation <- dataCommitById $ cid hFoundation
--    let lca = getLca dfromC dFoundation
--    --Single exit, single entry subgraph
--    let toRs = reverse $ (takeWhile (/= lca)) (ancestorList dfromC)
--    checkoutCom hFoundation
--    updateToRs toRs
--    rebase
--
applyConflictPatches :: WorkingTreeWriter m => ConflictPatches -> m ()
applyConflictPatches (CP confs noConfs) =
  let conflictPatches = map conflictAsPatch confs
      allPatches = sequenceParallelPatches (conflictPatches ++ noConfs)
  in applyFileTrans (applyPatches allPatches)

-- This is still ugly
--rebase :: m (Outcome ())
--rebase = do
--   hc <- (getHC >>= (dataCommitById . cid))
--   htoRs <- getToRs
--   dtoRs <- mapM (dataCommitById . cid) htoRs
--   case replay hc dtoRs of
--    (hc',Succ) -> addCommit hc' >> return Succ
--    (hc',Fail (CP confs noConfs,toRs')) ->
--      -- This is safe because it always succeeds when toRs is empty
--      let lca = getLca hc (head dtoRs)
--      in checkoutCom lca >> applyConflictPatches >> updateToRs toRs' >>
--         updateHead hc' >> return (Fail ())
--
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
      let filesA = Set.toList $ dContents dca
          filesB = Set.toList $ dContents dcb
          filesOnlyA = filesA List.\\ filesB
          filesOnlyB = filesB List.\\ filesA
      in patchFromFiles filesOnlyA filesOnlyB

-- return a patch representing going from files in a to files in b
-- Might be able to write this better with groupBy??
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

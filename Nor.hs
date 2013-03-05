module Nor where
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Crypto.Hash.SHA1 (hash)
import qualified Data.ByteString as BS
import qualified Control.Monad.State as S

import Core
import ObjectStore
import Patch

type ResolvedConflicts = ParallelPatches
data RebaseRes = Succ { core :: Core
                      , newHead :: Commit Hash }
               | Conf { core :: Core
                      , newHead :: Commit Hash
                      , cConfs :: [Conflict ParallelPatches]
                      , cNoConfs :: ParallelPatches
                      , ctoRebas :: [Commit Hash]
                      , cLca :: Commit Hash}
                      deriving (Show)

rebaseStart :: Core -> Commit Hash -> Commit Hash -> RebaseRes
rebaseStart core@(cs,_) fromC toC =
   let lca = getLca cs fromC toC
       toRs = reverse $ takeWhile (/= lca) (ancestorList cs fromC)
   in rebaseStep core toC toRs

-- the following functions are pure and can have QC properties
rebaseStep :: Core -> Commit Hash -> [Commit Hash] -> RebaseRes
rebaseStep core hc [] = Succ core hc
rebaseStep core@(cs,os) hc (tor:tors) =
    let lca = getLca cs hc tor
        (noConfs, confs) = mergeCommit os hc tor lca
    in if null confs
        then
            let stateWithCommit = parallelPatchesToCommit lca noConfs (cid hc)
                (head',core') = S.runState stateWithCommit core
            in rebaseStep core' head' tors
        else
            Conf core hc confs noConfs (tor:tors) lca -- KEEP tor when Conf

resolve :: Core -> Commit Hash -> ResolvedConflicts ->
           [Commit Hash] -> Commit Hash -> RebaseRes
resolve core hc rconfs toRs lca =
  let stateWithCommit = parallelPatchesToCommit lca rconfs (cid hc)
      (head',core') = S.runState stateWithCommit core
  in rebaseStep core' head' toRs

-- We expect toRs to not have been peeled off yet, creating an implicit commit
-- for the head of the torebase list
resolveWithFiles :: Core -> Commit Hash -> [File] -> [Commit Hash] -> RebaseRes
resolveWithFiles (core@(_,_os)) hc newFiles (_toR:toRs) =
    let (newHead,newCore) = S.runState (addCommit newFiles (cid hc)) core
    in rebaseStep newCore newHead toRs
resolveWithFiles _ _ _ [] = unimp "resolve with empty commit list"

unimp :: String -> a 
unimp s = error ("Not implemented: " ++ s)

addCommit :: [File] -> Hash -> S.State Core (Commit Hash)
addCommit fs pcid = S.state (\(cs, os) ->
    let (hs, os') = addObjects os fs
        c' = Commit (Just pcid) hs $ mkCommitHash (pcid:hs)
    in (c', (Set.insert c' cs, os')))
    where mkCommitHash :: [Hash] -> Hash
          mkCommitHash = Hash . hash . BS.concat . map getHash

commitById :: Set.Set (Commit Hash) -> Hash -> Maybe (Commit Hash)
commitById commitSet id =
    foldl (\mc c@(Commit _ _ cid) ->
                if id == cid then Just c
                             else mc) Nothing (Set.elems commitSet)

getLca :: Set.Set (Commit Hash) -> Commit Hash -> Commit Hash -> Commit Hash
getLca cs ca cb =
   let ancSeta = Set.fromList (ancestorList cs ca)
   in foldr (\a z -> if Set.member a ancSeta then a else z)
      (error "No LCA") (ancestorList cs cb)

ancestorList :: Set.Set (Commit Hash) -> Commit Hash -> [Commit Hash]
ancestorList _ c1@(Commit Nothing _ _) = [c1]
ancestorList cs c1@(Commit (Just pid) _ _) =
    let Just p = commitById cs pid
    in c1 : ancestorList cs p

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
patchFromCommits :: ObjectStore File -> Commit Hash -> Commit Hash -> ParallelPatches
patchFromCommits os ca cb =
      let hashesA = Set.fromList (cContents ca)
          hashesB = Set.fromList (cContents cb)
          onlyA = hashesA Set.\\ hashesB
          onlyB = hashesB Set.\\ hashesA
          filesOnlyA = getFilesForSet os onlyA
          filesOnlyB = getFilesForSet os onlyB
          in patchFromFiles filesOnlyA filesOnlyB
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

mergeCommit :: ObjectStore File -> Commit Hash -> Commit Hash -> Commit Hash ->
               (ParallelPatches,[Conflict ParallelPatches])
mergeCommit os ca cb lca =
      let patchTo = patchFromCommits os
          patchA = lca `patchTo` ca
          patchB = lca `patchTo` cb
      in patchA >||< patchB

parallelPatchesToCommit :: Commit Hash -> ParallelPatches -> Hash ->
                           S.State Core (Commit Hash)
parallelPatchesToCommit lca patches pcid = do
    c@(_, os) <- S.get
    let lcaFiles = fromJust $ mapM (getObject os) (cContents lca)
    let sPatches = sequenceParallelPatches patches
    let newFiles = applyPatches sPatches lcaFiles
    let (a, c') = S.runState (addCommit newFiles pcid) c
    S.put c'
    return a

module Nor where
import Control.Monad
import Control.Applicative
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Patch
import ObjectStore
import Crypto.Hash.SHA1 (hash)
import qualified Data.ByteString as Strict
import Data.Serialize
import qualified Control.Monad.State as S
---------------------------------

data File = File { path :: String -- Unix filepath: "/foo/bar/baz"
                 , contents :: [String] -- Simple representation for now
                 } deriving (Show,Eq)

instance Serialize File where
    put (File p c) = put p >> put c
    get = File <$> get <*> get

----------

class CoreReader a where
    readCore :: Core -> a

class (CoreReader a) => CoreWriter a where
    appendCore :: Core -> a -> Core

--data CR s a = CR { runState :: s -> (a, s) }
--instance Monad (CR s) where
--    return a = CR $ \s -> (a, s)
--    m >>= k = CR $ \z -> 
--                    let (a, s') = runState m z
--                    in runState (k a) s'

--data CR a = CR { readState :: Core -> a }
--instance Monad CR where
--    return a = CR $ \_ -> a
--    m >>= k = CR $ \z -> 
--                    let a = readState m z
--                    in readState (k a) z
--
--data CW a = CW { writeState :: Core -> (a, Core) }
--instance Monad CW where
--    return a = CW $ \_ -> (a, initCore)
--    m >>= k = CW $ \z -> 
--                    let (a, c') = writeState m z
--                    in writeState (k a) c'

--instance CoreReader CR where
--    readCore = 


--instance CoreReader R where
--    readCore = 

----------

--Mapping between Hashes -> a
type WithObjects a b = S.State (ObjectStore a) b

data Commit = Commit { parent :: Maybe Hash -- Initial commit has nothing
                     , hashes :: [Hash] -- Hashes of all files at given time
                     , cid :: Hash
                     } deriving (Show)

instance Ord Commit where
   compare c1 c2 = compare (cid c1) (cid c2)
instance Eq Commit where
   c1 == c2 = cid c1 == cid c2
instance Serialize Commit where
    put (Commit pid hs id) = put pid >> put hs >> put id
    get = Commit <$> get <*> get <*> get

-- list of all commits, hash->file, head commit, commitCount
type Core = (Set.Set Commit, ObjectStore File)

type ResolvedConflicts = ParallelPatches
data RebaseRes = Succ { core :: Core
                      , newHead :: Commit }
               | Conf { core :: Core
                      , newHead :: Commit
                      , cConfs :: [Conflict ParallelPatches]
                      , cNoConfs :: ParallelPatches
                      , ctoRebas :: [Commit]
                      , cLca :: Commit }
                      deriving (Show)

rebaseStart :: Core -> Commit -> Commit -> RebaseRes
rebaseStart core fromC toC =
   let lca = getLca core fromC toC
       toRs = reverse $ takeWhile (/= lca) (ancestorList core fromC)
   in rebaseStep core toC toRs

-- the following functions are pure and can have QC properties
rebaseStep :: Core -> Commit -> [Commit] -> RebaseRes
rebaseStep core hc [] = Succ core hc
rebaseStep core@(_,os) hc (tor:tors) =
    let lca = getLca core hc tor
        (noConfs, confs) = mergeCommit os hc tor lca
    in if null confs
        then
            let mergedC = parallelPatchesToCommit lca noConfs (Just (cid hc))
                (head',core') = S.runState (addCommit mergedC) core
            in rebaseStep core' head' tors
        else
            Conf core hc confs noConfs (tor:tors) lca -- KEEP tor when Conf

resolve :: Core -> Commit -> ResolvedConflicts ->
           [Commit] -> Commit -> RebaseRes
resolve core hc rconfs toRs lca =
  let mergedC = parallelPatchesToCommit lca rconfs (Just (cid hc))
      (head',core') = S.runState (addCommit mergedC) core
  in rebaseStep core' head' toRs

-- We expect toRs to not have been peeled off yet, creating an implicit commit
-- for the head of the torebase list
resolveWithFiles :: Core -> Commit -> [File] -> [Commit] -> RebaseRes
resolveWithFiles (core@(_,_os)) hc newFiles (_toR:toRs) =
    let hashableFs = addHashableAs newFiles
        newCommitWithFiles = createCommit hashableFs (Just hc)
        (newHead,newCore) = S.runState (addCommit newCommitWithFiles) core
    in rebaseStep newCore newHead toRs
resolveWithFiles _ _ _ [] = unimp "resolv with empty commit list"


unimp :: String -> a 
unimp s = error ("Not implemented: " ++ s)

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
    -- Build object store of all files in the commit to get the hashes
    let (_,commitOS) = S.runState s mkEmptyOS
    let hashes = getHashes commitOS
    -- Put the files into the objectsore given to the function
    newState <- S.get
    let (_,os) = S.runState s newState
    let Just pcid = liftM cid pc
    S.put os
    return $ Commit (Just pcid) hashes $ mkCommitHash (pcid:hashes)

addCommit :: WithObjects File Commit -> S.State Core Commit
addCommit s = S.state (\(commitS, os) ->
      let (newCommit,newOS) = S.runState s os
      in (newCommit, (Set.insert newCommit commitS, newOS)))

mkCommitHash :: [Hash] -> Hash
mkCommitHash = Hash . hash . Strict.concat . map getHash

-- An empty world
initCore :: Core
initCore = let initC = Commit Nothing [] $ Hash (hash (encode ""))
           in (Set.singleton initC, mkEmptyOS)

commitById :: Core -> Hash -> Maybe Commit
commitById (commitSet, _) id =
    foldl (\mc c@(Commit _ _ cid) ->
                if id == cid then Just c
                             else mc) Nothing (Set.elems commitSet)

medCheckout :: Core -> Commit -> Maybe [File]
medCheckout (_,os) (Commit _ hashes _) =
    mapM (getObject os) hashes

getLca :: Core -> Commit -> Commit -> Commit
getLca core ca cb =
   let ancSeta = Set.fromList (ancestorList core ca)
   in foldr (\a z -> if Set.member a ancSeta then a else z)
      (error "No LCA") (ancestorList core cb)

ancestorList :: Core -> Commit -> [Commit]
ancestorList _ c1@(Commit Nothing _ _) = [c1]
ancestorList core c1@(Commit (Just pid) _ _) =
    let Just p = commitById core pid
    in c1 : ancestorList core p

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
patchFromCommits :: ObjectStore File -> Commit -> Commit -> ParallelPatches
patchFromCommits os ca cb =
      let hashesA = Set.fromList (hashes ca)
          hashesB = Set.fromList (hashes cb)
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

mergeCommit :: ObjectStore File -> Commit -> Commit -> Commit ->
               (ParallelPatches,[Conflict ParallelPatches])
mergeCommit os ca cb lca =
      let patchTo = patchFromCommits os
          patchA = lca `patchTo` ca
          patchB = lca `patchTo` cb
      in patchA >||< patchB

parallelPatchesToCommit :: Commit -> ParallelPatches -> Maybe Hash ->
                           WithObjects File Commit
parallelPatchesToCommit lca patches mpcid = S.state (\os ->
      let lcaFiles = fromJust $ mapM (getObject os) (hashes lca)
          sPatches = sequenceParallelPatches patches
          newFiles = applyPatches sPatches lcaFiles
          (hs,newOS) = addObjects os newFiles
          commitHash = Hash $ hash $ Strict.concat (map getHash hs)
      in (Commit mpcid hs commitHash,newOS))

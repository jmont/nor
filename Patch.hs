module Patch where
import Data.Algorithm.Diff
import qualified Data.Map as Map
import Data.List
import Data.Graph as G
import Data.Tree as T

data Edit t = C -- Copy current input line to output
            | I t -- Insert argument line into output
            | D t -- Delete current input line which must match
            deriving Eq

type Path = String

data AtPath t = AP Path t

type Patch = AtPath PatchAction

data Conflict t = Conflict { firstConf :: t
                           , secondConf :: t
                           } deriving (Show)

data PatchAction = RemoveEmptyFile
                 | CreateEmptyFile
                 | Change ChangeHunk
                 deriving (Show, Eq, Ord)

data ChangeHunk = ChangeHunk { offset :: Int -- Starting Line Number
                             , old :: [String] -- List of old lines
                             , new :: [String] -- list of new lines
                             } deriving (Show, Eq)
instance Ord ChangeHunk where
   compare c1 c2 = compare (offset c1) (offset c2)

ppath :: Patch -> Path
ppath (AP p _) = p

patchAction :: Patch -> PatchAction
patchAction (AP _ pa) = pa

-- Like fromJust found in Maybe monad
fromChange :: PatchAction -> ChangeHunk
fromChange (Change ch) = ch
fromChange _ = error "fromChange applied to non-change hunk"

getEdits :: Eq t => [t] -> [t] -> [Edit t]
getEdits t1s t2s = map mapFun (getDiff t1s t2s)
   where mapFun :: (DI,t) -> Edit t
         mapFun (B,_) = C
         mapFun (F,t) = D t
         mapFun (S,t) = I t

applyEdits :: Eq t => [Edit t] -> [t] -> Maybe [t]
applyEdits es strs = sequence (aE es strs)
   where aE (C:es) (str2:strs) =
            Just str2 : aE es strs
         aE ((D str1):es) (str2:strs) =
            if (str1==str2) then aE es strs else [Nothing]
         aE ((I str1):es) strs = Just str1 : aE es strs
         aE [] [] = []
         aE _  [] = [Nothing]

editsToPatch :: [Edit String] -> Path -> [Patch]
editsToPatch es p = map (AP p . Change) (editsToChangeHunks es)

editsToChangeHunks :: [Edit String] -> [ChangeHunk]
editsToChangeHunks es = eTCH es 0
   where eqC = ((==) C)
         neqC = not . eqC
         eqD (D _) = True
         eqD _ = False
         neqD = not . eqD
         getStr (D str) = str
         getStr (I str) = str
         getStr C = error "this can't happen"
         eTCH es lineNum =
            let (keeps, rest) = span eqC es
                (changes, rest') = span neqC rest
                dels = map getStr (filter eqD changes)
                adds = map getStr (filter neqD changes)
                ch = ChangeHunk (lineNum + length keeps) dels adds
             in if (length adds + length dels) == 0
                 then []
                 else ch : eTCH rest' (offset ch + length dels)

--This is ugly and needs work
--ASSUMING NO CONFLICTS IN A PARALLEL PATCH SET
seqParallelPatches :: [Patch] -> [Patch]
seqParallelPatches [] = []
seqParallelPatches [p] = [p]
seqParallelPatches ps =
         let rems = filter eqRemEFile ps
             cres = filter eqCreEFile ps
             chs  = filter (\p -> not (eqRemEFile p || eqCreEFile p)) ps
         in cres ++ sortBy sortCh chs ++ rems
         where
         eqRemEFile (AP _ RemoveEmptyFile) = True
         eqRemEFile _ = False
         eqCreEFile (AP _ CreateEmptyFile) = True
         eqCreEFile _ = False
         sortCh :: Patch -> Patch -> Ordering
         sortCh (AP p1 ch1) (AP p2 ch2) =
            case compare p1 p2 of
               EQ -> compare (fromChange ch2) (fromChange ch1)  --Sort acesending
               otherwise  -> otherwise

(>||<) = mergeParallelPatches

mergeParallelPatches :: [Patch] -> [Patch] ->
                        ([Patch], [AtPath (Conflict [ChangeHunk])])
mergeParallelPatches p1s p2s =
      --Map Path [PatchAction]
  let pathAct1ByPath = foldr (\ps m -> Map.insert (ppath (head ps))
                              (map patchAction ps) m)
                              Map.empty (groupBy groupPatch p1s)
      pathAct2ByPath = foldr (\ps m -> Map.insert (ppath (head ps))
                              (map patchAction ps) m)
                              Map.empty (groupBy groupPatch p2s)
      --Map Path ([Patch],[Conflict [Patch]])
      possConfsByPath = Map.intersectionWithKey
                        (\path pa1s pa2s ->
                           let (noConfs,confs) = findConflictsPA pa1s pa2s
                               noConfPatches = map (AP path) noConfs
                               confPatches =
                                 map (\(Conflict pas pbs) ->
                                       AP path (Conflict pas pbs)) confs
                           in (noConfPatches,confPatches))
                        pathAct1ByPath pathAct2ByPath
      only1s = Map.difference pathAct1ByPath pathAct2ByPath
      only2s = Map.difference pathAct2ByPath pathAct1ByPath
      noConfsP1 = Map.foldrWithKey (\path pas patches ->
                                    map (AP path) pas ++ patches) [] only1s
      noConfsP2 = Map.foldrWithKey (\path pas patches ->
                                    map (AP path) pas ++ patches) [] only1s
      result = Map.fold (\(noConfs,confs) (allNoConfs,allConfs) ->
                          (noConfs ++ allNoConfs,confs ++ allConfs))
                         (noConfsP1 ++ noConfsP2,[]) possConfsByPath
      in result
  where groupPatch (AP p1 _) (AP p2 _) = p1 == p2

conflicts :: ChangeHunk -> ChangeHunk -> Bool
conflicts ch1 ch2
   | ch1 == ch2 = False -- Same hunk
   | offset ch1 == offset ch2 = True --Operate on the same lines
   | offset ch1 + length (old ch1) > offset ch2 = True -- 1 overlaps with 2
   | offset ch2 + length (old ch2) > offset ch1 = True -- 2 overlaps with 1
   | otherwise = False -- No conflict

--Works on a Path
findConflictsPA :: [PatchAction] -> [PatchAction] ->
                   ([PatchAction],[Conflict [ChangeHunk]])
--Remove empty files should be removed completely, just have the conflict
--delete the lines
findConflictsPA pas [] = (pas,[])
findConflictsPA [] pbs = (pbs,[])
findConflictsPA pas pbs =
   let aHasRem = any (== RemoveEmptyFile) pas
       bHasRem = any (== RemoveEmptyFile) pbs
       aHasCre = any (== CreateEmptyFile) pas
       bHasCre = any (== CreateEmptyFile) pbs
       aChangeH = map fromChange (filter isCH pas)
       bChangeH = map fromChange (filter isCH pbs)
       (chNoConf,chConfs) = getChangeHConfs aChangeH bChangeH
   in case (aHasRem,bHasRem) of
      (True,True) -> (filter (/= RemoveEmptyFile) (pas ++ pbs), [])
      (True,False) -> if any hasNew pbs
                      then ([],[Conflict aChangeH bChangeH])
                      else (pas ++ pbs,[])
      (False,True) -> findConflictsPA pbs pas
      (False,False) -> if aHasCre || bHasCre
                       then (CreateEmptyFile : map Change chNoConf,chConfs)
                       else (map Change chNoConf,chConfs)
   where isCH :: PatchAction -> Bool
         isCH (Change _) = True
         isCH _ = False
         hasNew (Change ch) = length (new ch) > 0
         hasNew _ = False

getChangeHConfs :: [ChangeHunk] -> [ChangeHunk] ->
                      ([ChangeHunk],[Conflict [ChangeHunk]])
getChangeHConfs ch1s ch2s =
   let confs1 = map (\ch -> (1,ch,(filter (conflicts ch) ch2s))) ch1s
       confs2 = map (\ch -> (2,ch,(filter (conflicts ch) ch1s))) ch2s
       (confGraph,adjList,keyToVertex) = G.graphFromEdges (confs1 ++ confs2)
       conflictTrees = G.components confGraph
   in  foldr (\confTree (noConfs,confs) ->
               let elems = flatten confTree
                   (fromPa1,fromPa2) = partition elems (== 1) adjList
               in if length elems == 1
                  then
                     let (_,ch,_) = adjList (head elems)
                     in (ch:noConfs,confs)
                  else (noConfs, Conflict fromPa1 fromPa2 : confs))
             ([],[]) conflictTrees
         --Detects conflicts within two lists of changehunks
   where partition :: [Vertex] -> (node -> Bool) ->
                     (Vertex -> (node,key,[key])) -> ([key],[key])
         partition vertexList partFun vertexMap =
            foldr (\(n,k,_) (k1s,k2s) ->
                     if partFun n then (k:k1s,k2s) else (k1s,k:k2s))
                  ([],[]) (map vertexMap vertexList)

--Doesn't introduce new conflicts with other stuff
--Sort them!
--conflictAsPatch :: Conflict -> Patch
--conflictAsPatch (Conflict cpath (pa1:pa1s) pa2s) = error "hI"

--Returns the olds for the conflict interval
getConflictOlds :: Conflict [ChangeHunk] -> [String]
getConflictOlds (Conflict (ch1:ch1s) (ch2:ch2s)) =
      let o1 = offset ch1
          o2 = offset ch2
      in if o1 <= o2 then gCO (o1,old ch1) ch1s (ch2:ch2s)
                     else gCO (o2,old ch2) (ch1:ch1s) ch2s
   where gCO :: (Int,[String]) -> [ChangeHunk] -> [ChangeHunk] -> [String]
         gCO (off,currOlds) [(ChangeHunk o1 old1s _)] [] =
            take (o1 - off) currOlds ++ old1s
         gCO (off,currOlds) [] [(ChangeHunk o2 old2s _)] =
            take (o2 - off) currOlds ++ old2s
         gCO (off,currOlds) (ch1@(ChangeHunk o1 old1s _):ch1s)
                            (ch2@(ChangeHunk o2 old2s _):ch2s) =
            if o1 <= o2
            then gCO (off, take (o1 - off) currOlds ++ old1s) ch1s (ch2:ch2s)
            else gCO (off, take (o2 - off) currOlds ++ old2s) (ch1:ch1s) ch2s
         gCO _ _ _ = error "Can't happen"

-- OLD getConflictASPatch
--     Patch cpath (vc pa1 pa1s pa2s)
--   where vc :: PatchAction -> [PatchAction] -> [PatchAction] -> PatchAction
--         vc accCh [c1] [] = addEquals $ mergeHunk accCh c1
--         vc accCh [] [c2] = addEquals $ mergeHunk accCh c2
--         vc accCh (c1@(ChangeHunk off1 _ _):c1s) (c2@(ChangeHunk off2 _ _):c2s)=
--            if (off1 <= off2)
--            then vc (mergeHunk accCh c1) c1s (c2:c2s)
--            else vc (mergeHunk accCh c2) (c1:c1s) c2s
--         vc accCh _ _ = error "This can't happen"
--         addEquals (ChangeHunk o dels news) =
--            ChangeHunk o dels (news ++ [">>>>>"])
--conflictAsPatch _ = error "First list of conflict empty"

--Changehunks must overlap maybe ensure this?
--mergeHunk :: PatchAction -> PatchAction -> PatchAction
--mergeHunk c1@(ChangeHunk off1 old1 new1) c2@(ChangeHunk off2 old2 new2) =
--   if off1 <= off2
--   then ChangeHunk off1 olds' ("<<<<<" : new1 ++ "=====" : new2)
--   else mergeHunk c2 c1
--   where olds' = take (off2 - off1) old1 ++ old2

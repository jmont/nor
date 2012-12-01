module Patch where
import Data.Algorithm.Diff
import qualified Data.Map as Map
import Data.List
import Data.Graph as G
import Data.Tree as T

type Edit t = (DI, t)
type Path = String

data Patch = Patch { ppath :: Path
                   , patchAction :: PatchAction
                   } deriving (Show)

data Conflict = Conflict { cpath :: Path
                         , firstPatch :: [PatchAction]
                         , secondPatch :: [PatchAction]
                         } deriving (Show)

data OrdHunk = Conf | Before | After deriving (Eq)

data PatchAction = RemoveEmptyFile
                 | CreateEmptyFile
                 | ChangeHunk { offset :: Int -- Starting Line Number
                              , old :: [String] -- List of old lines
                              , new :: [String] -- list of new lines
                              } deriving (Show, Eq, Ord)
--WE SHOULDN'T JUST DERIVE ORD!!

applyEdits :: Eq t => [Edit t] -> [t] -> Maybe [t]
applyEdits es strs = sequence (aE es strs)
   where aE ((B,str1):es) (str2:strs) =
            if (str1==str2) then Just str2 : aE es strs else [Nothing]
         aE ((F,str1):es) (str2:strs) =
            if (str1==str2) then aE es strs else [Nothing]
         aE ((S,str1):es) strs = Just str1 : aE es strs
         aE [] [] = []
         aE _  [] = [Nothing]

editsToPatch :: [Edit String] -> Path -> [Patch]
editsToPatch es p = map (Patch p) (editsToChangeHunks es)

editsToChangeHunks :: [Edit String] -> [PatchAction]
editsToChangeHunks es = eTCH es 0
   where eqB = ((==) B . fst)
         neqB = ((/=) B . fst)
         eqF = ((==) F . fst)
         neqF = ((/=) F . fst)
         eTCH es lineNum =
            let (keeps, rest) = span eqB es
                (changes, rest') = span neqB rest
                dels = map snd (filter eqF changes)
                adds = map snd (filter neqF changes)
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
         in cres ++ sortBy sortChs chs ++ rems
         where
         eqRemEFile (Patch _ RemoveEmptyFile) = True
         eqRemEFile _ = False
         eqCreEFile (Patch _ CreateEmptyFile) = True
         eqCreEFile _ = False
         sortCh :: Patch -> Patch -> Ordering
         sortCh (Patch p1 (ChangeHunk o1 _ _)) (Patch p2 (ChangeHunk o2 _ _)) =
            case compare p1 p2 of
               EQ -> compare o2 o1 --Sort acesending
               otherwise  -> otherwise
         sortChs _ _ = error "This can't happen"

(>||<) = mergeParallelPatches

mergeParallelPatches :: [Patch] -> [Patch] -> ([Patch], [Conflict])
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
                               noConfPatches = map (Patch path) noConfs
                               confPatches =
                                 map (\(pas,pbs) -> Conflict path pas pbs) confs
                           in (noConfPatches,confPatches))
                        pathAct1ByPath pathAct2ByPath
      only1s = Map.difference pathAct1ByPath pathAct2ByPath
      only2s = Map.difference pathAct2ByPath pathAct1ByPath
      noConfsP1 = Map.foldrWithKey (\path pas patches ->
                                    map (Patch path) pas ++ patches) [] only1s
      noConfsP2 = Map.foldrWithKey (\path pas patches ->
                                    map (Patch path) pas ++ patches) [] only1s
      result = Map.fold (\(noConfs,confs) (allNoConfs,allConfs) ->
                          (noConfs ++ allNoConfs,confs ++ allConfs))
                         (noConfsP1 ++ noConfsP2,[]) possConfsByPath
      in result
  where groupPatch p1 p2  = (ppath p1) == (ppath p2)

cmpHunk :: PatchAction -> PatchAction -> OrdHunk
cmpHunk (ChangeHunk o1 d1s _) (ChangeHunk o2 d2s _) =
   case compare o1 o2 of
      EQ -> Conf
      LT -> if o1 + length d1s > o2 then Conf else Before
      GT -> if o2 + length d2s > o1 then Conf else After
cmpHunk _ _ = error "Compare Hunk applied to non Change Hunks"

conflicts :: PatchAction -> PatchAction -> Bool
conflicts p1 p2 = cmpHunk p1 p2 == Conf

--Works on a Path
findConflictsPA :: [PatchAction] -> [PatchAction] ->
                   ([PatchAction],[([PatchAction],[PatchAction])])
--Remove empty files should be removed completely, just have the conflict
--delete the lines
findConflictsPA pas [] = (pas,[])
findConflictsPA [] pbs = (pbs,[])
findConflictsPA pas pbs =
   let aHasRem = any (== RemoveEmptyFile) pas
       bHasRem = any (== RemoveEmptyFile) pbs
       aHasCre = any (== CreateEmptyFile) pas
       bHasCre = any (== CreateEmptyFile) pbs
       (chNoConf,chConfs) = getChangeHConfs (filter isCH pas) (filter isCH pbs)
   in case (aHasRem,bHasRem) of
      (True,True) -> (filter (/= RemoveEmptyFile) (pas ++ pbs), [])
      (True,False) -> if any hasNew pbs
                      then ([],[(pas,pbs)])
                      else (pas ++ pbs,[])
      (False,True) -> findConflictsPA pbs pas
      (False,False) -> if aHasCre || bHasCre
                       then (CreateEmptyFile : chNoConf,chConfs)
                       else (chNoConf,chConfs)
   where isCH (ChangeHunk _ _ _) = True
         isCH _ = False
         hasNew (ChangeHunk off olds news) = length news > 0
         hasNew _ = False

getChangeHConfs :: [PatchAction] -> [PatchAction] ->
                      ([PatchAction],[([PatchAction],[PatchAction])])
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
                  else (noConfs, (fromPa1,fromPa2) : confs))
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
conflictAsPatch :: Conflict -> Patch
conflictAsPatch (Conflict cpath (pa1:pa1s) pa2s) =
      Patch cpath (vc pa1 pa1s pa2s)
   where vc :: PatchAction -> [PatchAction] -> [PatchAction] -> PatchAction
         vc accCh [c1] [] = addEquals $ mergeHunk accCh c1
         vc accCh [] [c2] = addEquals $ mergeHunk accCh c2
         vc accCh (c1@(ChangeHunk off1 _ _):c1s) (c2@(ChangeHunk off2 _ _):c2s)=
            if (off1 <= off2)
            then vc (mergeHunk accCh c1) c1s (c2:c2s)
            else vc (mergeHunk accCh c2) (c1:c1s) c2s
         vc accCh _ _ = error "This can't happen"
         addEquals (ChangeHunk o dels news) =
            ChangeHunk o dels (news ++ ["====="])
conflictAsPatch _ = error "First list of conflict empty"

--Changehunks must overlap maybe ensure this?
mergeHunk :: PatchAction -> PatchAction -> PatchAction
mergeHunk c1@(ChangeHunk off1 old1 new1) c2@(ChangeHunk off2 old2 new2) =
   if off1 <= off2
   then ChangeHunk off1 olds' ("<<<<<" : new1 ++ ">>>>>" : new2)
   else mergeHunk c2 c1
   where olds' = take (off1 - off2) old1 ++ old2

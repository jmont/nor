module Patch where
import Data.Algorithm.Diff
import qualified Data.Map as Map
import Data.List
import Data.Graph as G
import Data.Tree as T

data Edit t = C -- Copy current input line to output
            | I t -- Insert argument line into output
            | D t -- Delete current input line which must match
            deriving (Show, Eq)

type Path = String

data AtPath t = AP Path t deriving (Eq, Show)

type Patch = AtPath PatchAction

type ParallelPatches = [Patch]

newtype SequentialPatch = SP Patch deriving Eq

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

data Conflict t = Conflict { firstConf :: t
                           , secondConf :: t
                           } deriving (Show, Eq)

class Conflictable t where
    conflicts :: t -> t -> Bool

instance Conflictable ChangeHunk where
    conflicts ch1 ch2
       | ch1 == ch2 = False -- Same hunk
       | offset ch1 == offset ch2 = True -- Operate on the same lines
       | offset ch1 < offset ch2 =
          offset ch1 + length (old ch1) > offset ch2 -- 1 overlaps with 2
       | offset ch1 > offset ch2 =
          offset ch2 + length (old ch2) > offset ch1 -- 2 overlaps with 1


ppath :: Patch -> Path
ppath (AP p _) = p

patchAction :: Patch -> PatchAction
patchAction (AP _ pa) = pa

eqC :: Eq t => Edit t -> Bool
eqC = (C ==)

eqD :: Eq t => Edit t -> Bool
eqD (D _) = True
eqD _ = False

-- Like fromJust found in Maybe monad
fromChange :: PatchAction -> ChangeHunk
fromChange (Change ch) = ch
fromChange _ = error "fromChange applied to non-change hunk"

getEdits :: Eq t => [t] -> [t] -> [Edit t]
getEdits t1s t2s = toCanonical $ map mapFun $ getDiff t1s t2s
   where toCanonical [] = []
         toCanonical es =
             let (keeps, rest) = span eqC es
                 (changes, rest') = span (not . eqC) rest
                 dels = filter eqD changes
                 adds = filter (not . eqD) changes
             in keeps ++ dels ++ adds ++ toCanonical rest'

         mapFun :: (DI,t) -> Edit t
         mapFun (B,_) = C
         mapFun (F,t) = D t
         mapFun (S,t) = I t

applyEdits :: (Show t, Eq t) => [Edit t] -> [t] -> [t]
applyEdits es strs = aE es strs
   where aE (C:es) (str2:strs) =
            str2 : aE es strs
         aE (D str1 : es) (str2:strs) =
            if str1 == str2 then aE es strs else error "Deletes don't match"
         aE (I str1 : es) strs = str1 : aE es strs
         aE [] [] = []
         aE es strs = error ("Bad things happened: es:" ++ show es ++
                            " and strs:" ++ show strs)

editsToPatch :: [Edit String] -> Path -> ParallelPatches
editsToPatch es p = map (AP p . Change) (editsToChangeHunks es)

editsToChangeHunks :: [Edit String] -> [ChangeHunk]
editsToChangeHunks es = eTCH es 0
   where getStr (D str) = str
         getStr (I str) = str
         getStr C = error "this can't happen"

         eTCH es lineNum =
            let (keeps, rest) = span eqC es
                (changes, rest') = span (not . eqC) rest
                (dels, adds)  = span eqD changes
                ch = ChangeHunk (lineNum + length keeps)
                        (map getStr dels) (map getStr adds)
             in if (length adds + length dels) == 0
                 then []
                 else ch : eTCH rest' (offset ch + length dels)

--Needs Sorted!!
--minoff is the minimum
changeHunksToEdits :: [ChangeHunk] -> Int -> Int -> [Edit String]
changeHunksToEdits [] csToAdd _ = take csToAdd (repeat C)
changeHunksToEdits chs fileLength minoff =
   let edits = cHE 0 [] chs
       lastCh = last chs
       -- Pad the end with Cs. Use minoff because CHs refer to absolute position
       -- in original file and minoff adjusts for that.
       csToAdd = fileLength - (offset lastCh - minoff) - length (old lastCh)
   in edits ++ take csToAdd (repeat C)
   where cHE :: Int -> [Edit String] -> [ChangeHunk] -> [Edit String]
         cHE off es [] = es
         cHE off es (ch:chs) =
            let cs = take (offset ch - off) (repeat C)
                is = map I (new ch)
                ds = map D (old ch)
            in cHE (offset ch + length (old ch)) (es ++ cs ++ ds ++ is) chs

--This is ugly and needs work
--ASSUMING NO CONFLICTS IN A PARALLEL PATCH SET
sequenceParallelPatches :: ParallelPatches -> [SequentialPatch]
sequenceParallelPatches [] = []
sequenceParallelPatches [p] = [SP p]
sequenceParallelPatches ps =
         let rems = filter eqRemEFile ps
             cres = filter eqCreEFile ps
             chs  = filter (\p -> not (eqRemEFile p || eqCreEFile p)) ps
         in map SP $ cres ++ sortBy sortCh chs ++ rems
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

mergeParallelPatches :: ParallelPatches -> ParallelPatches ->
                        (ParallelPatches, [AtPath (Conflict [ChangeHunk])])
mergeParallelPatches p1s p2s =
      --Map Path [PatchAction]
  let pathAct1ByPath = foldr (\ps m -> Map.insert (ppath (head ps))
                              (map patchAction ps) m)
                              Map.empty (groupBy groupPatch p1s)
      pathAct2ByPath = foldr (\ps m -> Map.insert (ppath (head ps))
                              (map patchAction ps) m)
                              Map.empty (groupBy groupPatch p2s)
      --Map Path (ParallelPatches,[Conflict ParallelPatches])
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
   let confs1 = map (\ch -> (1, ch, filter (conflicts ch) ch2s)) ch1s
       confs2 = map (\ch -> (2, ch, filter (conflicts ch) ch1s)) ch2s
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
conflictAsPatch :: AtPath (Conflict [ChangeHunk]) -> Patch
conflictAsPatch (AP p conf) = AP p $ Change $ conflictAsCH conf

conflictAsCH :: Conflict [ChangeHunk] -> ChangeHunk
conflictAsCH (c@(Conflict uch1s uch2s)) =
   let (ch1s, ch2s) = (sort uch1s, sort uch2s)
       olds = getConflictOlds c
       off = min (offset (head ch1s)) (offset (head ch2s))
       editsCh1 = drop off $ changeHunksToEdits ch1s (length olds) off
       editsCh2 = drop off $ changeHunksToEdits ch2s (length olds) off
       appliedCh1 = applyEdits editsCh1 olds
       appliedCh2 = applyEdits editsCh2 olds
   in ChangeHunk off olds
         (("<<<<<" : appliedCh1) ++ ("=====" : appliedCh2) ++ [">>>>>"])

--Returns the olds for the conflict interval
getConflictOlds :: Conflict [ChangeHunk] -> [String]
getConflictOlds (Conflict [] []) = error "Empty conflicts"
getConflictOlds (Conflict ch1s ch2s) =
    let (fch:rest) = sort $ ch1s ++ ch2s
    in gCO' (offset fch) (old fch) rest
    where gCO' :: Int -> [String] -> [ChangeHunk] -> [String]
          gCO' off currOlds [] = currOlds
          gCO' off currOlds (ChangeHunk o olds news:chs)
            | (length currOlds + off) > (o + length olds) = gCO' off currOlds chs
            | otherwise = gCO' off (take (o - off) currOlds ++ olds) chs

--conflictAsPatchIO :: AtPath (Conflict [ChangeHunk]) -> IO Patch
--conflictAsPatchIO (AP cpath (c@(Conflict ch1s ch2s))) = do
--   let olds = getConflictOlds c
--   let off = min (offset (head ch1s)) (offset (head ch2s))
--   let editsCh1 = drop off $ changeHunksToEdits ch1s (length olds) off
--   putStrLn $ "off:" ++ show off
--   putStrLn $ "ch2:" ++ show (head (tail ch1s))
--   putStrLn $ "FOO:" ++ show editsCh1
--   let editsCh2 = drop off $ changeHunksToEdits ch2s (length olds) off
--   let appliedCh1 = applyEdits editsCh1 olds
--   let appliedCh2 = applyEdits editsCh2 olds
--   putStrLn $ "olds:" ++  show olds
--   return $ AP cpath $ Change $ ChangeHunk off olds
--         (("<<<<<" : appliedCh1) ++ ("=====" : appliedCh2) ++ [">>>>>"])

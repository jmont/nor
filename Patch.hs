module Patch where
import Data.Algorithm.Diff
import qualified Data.Map as Map
import Data.List
import Data.Graph as G
import Data.Tree as T
import qualified Data.Set as Set

data Edit t = C -- Copy current input line to output
            | I t -- Insert argument line into output
            | D t -- Delete current input line which must match
            deriving (Show, Eq)

type Path = String

data AtPath t = AP Path t deriving (Eq, Show)

type Patch = AtPath PatchAction

type ParallelPatches = [Patch]

newtype SequentialPatch = SP Patch deriving Eq

data PatchAction = RemoveFile [String] -- File contents to delete
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

instance Ord t => Ord (AtPath t) where
   compare (AP p1 t1) (AP p2 t2) = case compare p1 p2 of
       EQ -> compare t1 t2
       notEq -> notEq

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

-- When two patchactions applied to the same path conflict
instance Conflictable PatchAction where
   conflicts (RemoveFile _) (RemoveFile _) = False
   conflicts CreateEmptyFile CreateEmptyFile = False
   conflicts (Change ch1) (Change ch2) = conflicts ch1 ch2
   conflicts _ _ = True

instance Conflictable t => Conflictable (AtPath t) where
   conflicts (AP p1 t1) (AP p2 t2) = p1 == p2 && conflicts t1 t2

appath :: AtPath t -> Path
appath (AP p _) = p

fromPath :: AtPath t -> t
fromPath (AP _ t) = t

ungroupByPath :: [AtPath [t]] -> [AtPath t]
ungroupByPath apts = concatMap moveAPIn apts
    where moveAPIn :: AtPath [t] -> [AtPath t]
          moveAPIn (AP p ts) = map (AP p) ts

groupByPath :: [AtPath t] -> [AtPath [t]]
groupByPath aps =
    let apls = groupBy (\(AP p1 _) (AP p2 _) -> p1 == p2) aps
    in map moveAPOut apls
    where moveAPOut :: [AtPath t] -> AtPath [t]
          moveAPOut (aps@(a:_)) = AP (appath a) $ map fromPath aps

-- Like fromJust found in Maybe monad
fromChange :: PatchAction -> ChangeHunk
fromChange (Change ch) = ch
fromChange _ = error "fromChange applied to non-change hunk"

getEdits :: Eq t => [t] -> [t] -> [Edit t]
getEdits t1s t2s = toCanonical $ map mapFun $ getDiff t1s t2s
   where toCanonical [] = []
         toCanonical es =
             let (keeps, rest) = span eqC es
                 (changes, rest') = span neqC rest
                 dels = filter eqD changes
                 adds = filter neqD changes
             in keeps ++ dels ++ adds ++ toCanonical rest'

         mapFun :: (DI,t) -> Edit t
         mapFun (B,_) = C
         mapFun (F,t) = D t
         mapFun (S,t) = I t

         eqC = ((==) C)
         neqC = not . eqC
         eqD (D _) = True
         eqD _ = False
         neqD = not . eqD

applyEdits :: (Show t, Eq t) => [Edit t] -> [t] -> [t]
applyEdits es strs = aE es strs
   where aE (C:es) (str2:strs) =
            str2 : aE es strs
         aE ((D str1):es) (str2:strs) =
            if (str1 == str2) then aE es strs else error "Deletes don't match"
         aE ((I str1):es) strs = str1 : aE es strs
         aE [] [] = []
         aE es strs = error ("Bad things happened: es:" ++ show es ++
                            " and strs:" ++ show strs)

editsToPatch :: [Edit String] -> Path -> ParallelPatches
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
         eqRemEFile (AP _ (RemoveFile _)) = True
         eqRemEFile _ = False
         eqCreEFile (AP _ CreateEmptyFile) = True
         eqCreEFile _ = False

         sortCh :: Patch -> Patch -> Ordering
         sortCh (AP p1 ch1) (AP p2 ch2) =
            case compare p1 p2 of
               EQ -> compare (fromChange ch2) (fromChange ch1)  --Sort acesending
               otherwise  -> otherwise

p1s >||< p2s =
    let (noConfs, confs) = mergeParallelPatches p1s p2s
    in (noConfs, map confPPToConfCH confs)

--Doesn't introduce new conflicts with other stuff
conflictAsPatch :: AtPath (Conflict [ChangeHunk]) -> Patch
conflictAsPatch (AP p (c@(Conflict uch1s uch2s))) =
   let (ch1s, ch2s) = (sort uch1s, sort uch2s)
       olds = getConflictOlds c
       off = min (offset (head ch1s)) (offset (head ch2s))
       editsCh1 = drop off $ changeHunksToEdits ch1s (length olds) off
       editsCh2 = drop off $ changeHunksToEdits ch2s (length olds) off
       appliedCh1 = applyEdits editsCh1 olds
       appliedCh2 = applyEdits editsCh2 olds
   in AP p $ Change $ ChangeHunk off olds
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

-- Helper function to post-process the conflicts
confPPToConfCH :: Conflict ParallelPatches -> AtPath (Conflict [ChangeHunk])
confPPToConfCH (Conflict p1s p2s) =
   let ch1s = onlyChs p1s []
       ch2s = onlyChs p2s []
   in AP (appath (head p1s)) $ Conflict ch1s ch2s
   where onlyChs :: ParallelPatches -> [ChangeHunk] -> [ChangeHunk]
         onlyChs (AP _ (Change ch) : ps) currChs =
            onlyChs ps (ch:currChs)
         onlyChs (_:ps) currChs = onlyChs ps currChs
         onlyChs [] currChs = currChs

mergeParallelPatches :: ParallelPatches -> ParallelPatches ->
                      (ParallelPatches,[Conflict ParallelPatches])
mergeParallelPatches p1s p2s =
   let confs1 = map (\p -> (1,p,(filter (conflicts p) p2s))) p1s
       confs2 = map (\p -> (2,p,(filter (conflicts p) p1s))) p2s
       (confGraph,adjList,keyToVertex) = G.graphFromEdges (confs1 ++ confs2)
       conflictTrees = G.components confGraph
       (noConfs,confs) = foldr (\confTree (noConfs,confs) ->
               let elems = flatten confTree
                   (fromP1,fromP2) = partition elems (== 1) adjList
               in if length elems == 1
                  then
                     let (_,ch,_) = adjList (head elems)
                     in (ch:noConfs,confs)
                  else (noConfs, Conflict fromP1 fromP2 : confs))
             ([],[]) conflictTrees
   in (Set.toList (Set.fromList noConfs), confs)
         --Detects conflicts within two lists of changehunks
   where partition :: [Vertex] -> (node -> Bool) ->
                     (Vertex -> (node,key,[key])) -> ([key],[key])
         partition vertexList partFun vertexMap =
            foldr (\(n,k,_) (k1s,k2s) ->
                     if partFun n then (k:k1s,k2s) else (k1s,k:k2s))
                  ([],[]) (map vertexMap vertexList)

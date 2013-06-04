module Patch where
import Data.Algorithm.Diff
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

newtype SequentialPatch = SP Patch deriving (Eq, Show)

data PatchAction = RemoveFile [String] -- File contents to delete
                 | CreateFile [String] -- File contents to add
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
       | offset ch1 == offset ch2 = True -- Operate on the same lines
       | offset ch1 < offset ch2 =
          offset ch1 + length (old ch1) > offset ch2 -- 1 overlaps with 2
       | offset ch1 > offset ch2 =
          offset ch2 + length (old ch2) > offset ch1 -- 2 overlaps with 1
       | otherwise = error "change hunks not ==, <, or >"

-- When two patchactions applied to the same path conflict
instance Conflictable PatchAction where
   conflicts (Change ch1) (Change ch2) = conflicts ch1 ch2
   conflicts _ _ = True

instance Conflictable t => Conflictable (AtPath t) where
   conflicts (AP p1 t1) (AP p2 t2) = p1 == p2 && conflicts t1 t2

appath :: AtPath t -> Path
appath (AP p _) = p

fromPath :: AtPath t -> t
fromPath (AP _ t) = t

isCH :: Patch -> Bool
isCH (AP _ (Change _)) = True
isCH _ = False

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
                 (changes, rest') = break eqC rest
                 dels = filter eqD changes
                 adds = filter (not . eqD) changes
             in keeps ++ dels ++ adds ++ toCanonical rest'
         mapFun :: Diff t -> Edit t
         mapFun (Both _ _) = C
         mapFun (First t) = D t
         mapFun (Second t) = I t

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
                (changes, rest') = break eqC rest
                (dels, adds)  = span eqD changes
                ch = ChangeHunk (lineNum + length keeps)
                        (map getStr dels) (map getStr adds)
             in if (length adds + length dels) == 0
                 then []
                 else ch : eTCH rest' (offset ch + length dels)

--Needs Sorted!!
--minoff is the minimum
changeHunksToEdits :: [ChangeHunk] -> Int -> Int -> [Edit String]
changeHunksToEdits [] csToAdd _ = replicate csToAdd C
changeHunksToEdits chs fileLength minoff =
   let edits = cHE 0 [] chs
       lastCh = last chs
       -- Pad the end with Cs. Use minoff because CHs refer to absolute position
       -- in original file and minoff adjusts for that.
       csToAdd = fileLength - (offset lastCh - minoff) - length (old lastCh)
   in edits ++ replicate csToAdd C
   where cHE :: Int -> [Edit String] -> [ChangeHunk] -> [Edit String]
         cHE _ es [] = es
         cHE off es (ch:chs) =
            let cs = replicate (offset ch - off) C
                is = map I (new ch)
                ds = map D (old ch)
            in cHE (offset ch + length (old ch)) (es ++ cs ++ ds ++ is) chs

--ASSUMING NO CONFLICTS IN A PARALLEL PATCH SET
sequenceParallelPatches :: ParallelPatches -> [SequentialPatch]
sequenceParallelPatches = map SP . reverse . sort

(>||<)  :: ParallelPatches -> ParallelPatches -> (ParallelPatches, [Conflict ParallelPatches])
(>||<) = mergeParallelPatches

-- We know that conflicting parallelpatches must all act on same path
conflictAsPatch :: Conflict ParallelPatches -> Patch
conflictAsPatch (Conflict [] _) = error "empty list in conflict"
conflictAsPatch (Conflict _ []) = error "empy list in conflict"
conflictAsPatch (Conflict [p1] [p2]) =
  if p1 == p2
    then p1
    else conflictAsPatch' (AP (appath p1) (Conflict [fromPath p1] [fromPath p2]))
conflictAsPatch (Conflict p1s p2s) =
   let p = appath $ head p1s --Conflict list should never be empty
       pa1s = map fromPath p1s
       pa2s = map fromPath p2s
   in conflictAsPatch' (AP p (Conflict pa1s pa2s))

--Doesn't introduce new conflicts with other stuff
conflictAsPatch' :: AtPath (Conflict [PatchAction]) -> Patch
conflictAsPatch' (AP p (Conflict [CreateFile c1] [CreateFile c2])) =
   AP p $ CreateFile $ "<<<<<" : c1 ++ "=====" : c2 ++ [">>>>>"]
conflictAsPatch' (AP p (Conflict [RemoveFile c] ps)) =
    conflictAsPatch' (AP p (Conflict [Change (ChangeHunk 0 c [])] ps))
conflictAsPatch' (AP p (Conflict ps [RemoveFile c])) =
    conflictAsPatch' (AP p (Conflict ps [Change (ChangeHunk 0 c [])]))
conflictAsPatch' (AP p (Conflict uch1s uch2s)) =
    let (ch1s, ch2s) = (sort (map fromChange uch1s), sort (map fromChange uch2s))
        olds = getConflictOlds $ Conflict ch1s ch2s
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
          gCO' _ currOlds [] = currOlds
          gCO' off currOlds (ChangeHunk o olds _:chs)
            | (length currOlds + off) > (o + length olds) = gCO' off currOlds chs
            | otherwise = gCO' off (take (o - off) currOlds ++ olds) chs

mergeParallelPatches :: ParallelPatches -> ParallelPatches ->
                      (ParallelPatches,[Conflict ParallelPatches])
mergeParallelPatches p1s p2s =
   let confs1 = map (\p -> (Left (), p, filter (conflicts p) p2s)) p1s
       confs2 = map (\p -> (Right (), p, filter (conflicts p) p1s)) p2s
       (confGraph,adjList,_) = G.graphFromEdges (confs1 ++ confs2)
       conflictTrees = G.components confGraph
       (noConfs,confs) = foldr (\confTree (noConfs,confs) ->
               let elems = flatten confTree
                   (fromPP1,fromPP2) = partition elems (== Left ()) adjList
               in case elems of
                    [] -> error "Empty connected component"
                    [unconnectedV] ->
                      let (_,ch,_) = adjList unconnectedV
                      in (ch:noConfs,confs)
                    _connected -> (noConfs, Conflict fromPP1 fromPP2 : confs))
             ([],[]) conflictTrees
   in (noConfs, confs)
         --Detects conflicts within two lists of changehunks
   where partition :: [Vertex] -> (node -> Bool) ->
                     (Vertex -> (node,key,[key])) -> ([key],[key])
         partition vertexList partFun vertexMap =
            foldr (\(n,k,_) (k1s,k2s) ->
                     if partFun n then (k:k1s,k2s) else (k1s,k:k2s))
                  ([],[]) (map vertexMap vertexList)

adjustedByPatch :: Patch -> Patch -> Patch
pToAdj `adjustedByPatch` pC
  | conflicts pC pToAdj =
     error $ "adjustPatch got conflicting patches" ++ show pC ++ " " ++ show pToAdj
  | otherwise =
     case (pC,pToAdj) of
       (AP _ (Change ch1),AP pToAdj (Change ch2)) ->
          AP pToAdj $ Change $ ch2 `adjustedByCH` ch1
       _ -> pToAdj
  where adjustedByCH :: ChangeHunk -> ChangeHunk -> ChangeHunk
        ch2 `adjustedByCH` (ChangeHunk off1 old1 new1)
          | off1 < offset ch2 =
            ChangeHunk (length new1 - length old1 + offset ch2) (old ch2) (new ch2)
          | otherwise = ch2

adjustedByPPatch :: ParallelPatches -> ParallelPatches -> ParallelPatches
ppToAdj `adjustedByPPatch` ppC = map (\p -> foldr (flip adjustedByPatch) p ppC) ppToAdj

adjustedByPatchConf :: Patch -> Patch -> Either Patch (Conflict Patch)
pToAdj `adjustedByPatchConf` pC
  | conflicts pC pToAdj =
      Right (Conflict pC pToAdj)
  | otherwise =
     case (pC,pToAdj) of
       (AP _ (Change ch1),AP pToAdj (Change ch2)) ->
          Left $ AP pToAdj $ Change $ ch2 `adjustedByCH` ch1
       _ -> Left pToAdj
  where adjustedByCH :: ChangeHunk -> ChangeHunk -> ChangeHunk
        chToAdj `adjustedByCH` (ChangeHunk off1 old1 new1)
          | off1 < offset chToAdj =
            ChangeHunk (length new1 - length old1 + offset chToAdj) (old chToAdj) (new chToAdj)
          | otherwise = chToAdj

adjustedByPPatchConf :: ParallelPatches -> ParallelPatches ->
                        (ParallelPatches,[Conflict ParallelPatches])
ppToAdj `adjustedByPPatchConf` ppC =
  let (noConfs,confs) = ppToAdj `adjustHelper` ppC
  in (noConfs,asMaxConflictSets confs)
  where adjustHelper :: ParallelPatches -> ParallelPatches ->
                        (ParallelPatches,[Conflict Patch])
        [] `adjustHelper` _ = ([],[])
        (pToAdj:pToAdjs) `adjustHelper` ppC =
          let (adjP2,confp2) = foldr (\pC (p,confs) -> case p `adjustedByPatchConf` pC
                                                      of Left newp -> (newp,confs)
                                                         Right newConf -> (p, newConf:confs))
                                                     (pToAdj,[]) ppC
              (adjP2s,confp2s) = pToAdjs `adjustHelper` ppC
          in (adjP2:adjP2s,confp2 ++ confp2s)

asMaxConflictSets :: [Conflict Patch] -> [Conflict ParallelPatches]
asMaxConflictSets confPatches =
  --This is a slow way of doing it
  let (noConfs,confs) = uncurry mergeParallelPatches $ unzipConfs confPatches
  in if null noConfs then confs else error "recived a patch not in conflict"
  where unzipConfs :: [Conflict Patch] -> (ParallelPatches,ParallelPatches)
        unzipConfs [] = ([],[])
        unzipConfs (Conflict p1 p2 : confs) =
          let (p1s,p2s) = unzipConfs confs
          in (p1:p1s,p2:p2s)

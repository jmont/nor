module Patch where
import Data.Algorithm.Diff
import Data.List

type Edit t = (DI, t)
type Path = String

data Patch = Patch Path PatchAction deriving (Show)

data Conflict t = Conflict t t

data OrdHunk = Conf | Before | After

data PatchAction = RemoveEmptyFile
                 | CreateEmptyFile
                 | ChangeHunk { offset :: Int -- Starting Line Number
                              , old :: [String] -- List of old lines
                              , new :: [String] -- list of new lines
                              } deriving (Show, Eq)

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
sequenceParallelPatches :: [Patch] -> [Patch]
sequenceParallelPatches [] = []
sequenceParallelPatches [p] = [p]
sequenceParallelPatches ps =
         let rems = filter eqRemEFile ps
             cres = filter eqCreEFile ps
             chs  = filter (\p -> not (or [(eqRemEFile p),(eqCreEFile p)])) ps
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

flattenPatches :: [Patch] -> [Patch]
flattenPatches [] = []
flattenPatches ((Atomic ps):ps') = ps ++ flattenPatches ps'
flattenPatches (p:ps') = p : flattenPatches ps'

mergeParallelPatches :: Patch -> Patch -> (Patch, [Conflict [Patch]])
mergeParallelPatches (Atomic p1s) (Atomic p2s) = 
   let  = groupBy groupFun p1s
        
   where confPAtoConfP :: Conflict [PatchAction] -> Path -> Conflict Patch
         confPAtoConfP (Conflict pa1s pa2s) p =
            Conflict (Atomic (map (AtPath p) pa1s)) 
                     (Atomic (map (AtPath p) pa2s))
         groupFun (AtPath p1 _) (AtPath p2 _) = p1 == p2
         groupFun _ _ = False

cmpHunk :: PatchAction -> PatchAction -> OrdHunk
cmpHunk (ChangeHunk o1 d1s _) (ChangeHunk o2 d2s _) =
   case compare o1 o2 of
      EQ -> Conf
      LT -> if o1 + length d1s > o2 then Conf else Before
      GT -> if o2 + length d2s > o1 then Conf else After
cmpHunk _ _ = error "Compare Hunk applied to non Change Hunks"

conflicts :: PatchAction -> PatchAction -> Bool
conflicts p1 p2 = case cmpHunk p1 p2 of 
      Conf -> True
      _ -> False

findConflictsPA :: [PatchAction] -> [PatchAction] -> 
                   ([PatchAction],[Conflict [PatchAction]])
findConflictsPA pas [] = (pas,[])
findConflictsPA [] pbs = (pbs,[])
findConflictsPA pas pbs =
   let aHasRem = any (== RemoveEmptyFile) pas 
       bHasRem = any (== RemoveEmptyFile) pbs
       aHasCre = any (== CreateEmptyFile) pas
       bHasCre = any (== CreateEmptyFile) pbs
       (chNoConf,chConfs) = confCHs (filter isCH pas) (filter isCH pbs)
   in case (aHasRem,bHasRem) of
      (True,True) -> (filter (/= RemoveEmptyFile) (pas ++ pbs), [])
      (True,False) -> if any hasNew pbs 
                      then ([],[Conflict pas pbs])
                      else (pas ++ pbs,[])
      (False,True) -> findConflictsPA pbs pas
      (False,False) -> if or [aHasCre,bHasCre] 
                       then (CreateEmptyFile : chNoConf,chConfs)
                       else (chNoConf,chConfs)
   where isCH (ChangeHunk _ _ _) = True
         isCH _ = False
         hasNew (ChangeHunk off olds news) = length news > 0
         hasNew _ = False
         confCHs :: [PatchAction] -> [PatchAction] -> 
                    ([PatchAction],[Conflict [PatchAction]])
         confCHs [] c2s = (c2s,[])
         confCHs (c1:c1s) c2s =
            let confs = filter (conflicts c1) c2s
                (noConfs,allConfs) = confCHs c1s c2s
            in if null confs then (c1:noConfs, allConfs)
                             else (noConfs, Conflict [c1] confs : allConfs)

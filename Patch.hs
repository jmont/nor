module Patch where
import Data.Algorithm.Diff
import Data.List

type Edit t = (DI, t)
type Path = String

data Patch = AtPath Path PatchAction
           | Atomic [Patch] deriving (Show)

data PatchAction = RemoveEmptyFile
                 | CreateEmptyFile
                 | ChangeHunk { offset :: Int -- Starting Line Number
                              , old :: [String] -- List of old lines
                              , new :: [String] -- list of new lines
                              } deriving (Show)

applyEdits :: Eq t => [Edit t] -> [t] -> Maybe [t]
applyEdits es strs = sequence (aE es strs)
   where aE ((B,str1):es) (str2:strs) =
            if (str1==str2) then Just str2 : aE es strs else [Nothing]
         aE ((F,str1):es) (str2:strs) =
            if (str1==str2) then aE es strs else [Nothing]
         aE ((S,str1):es) strs = Just str1 : aE es strs
         aE [] [] = []
         aE _  [] = [Nothing]

editsToPatch :: [Edit String] -> Path -> Patch
editsToPatch es p = Atomic $ map (AtPath p) (editsToChangeHunks es)

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
sequenceParallelPatches :: Patch -> Patch
sequenceParallelPatches p@(AtPath _ _) = p
sequenceParallelPatches (Atomic ps) =
         let ps' = flattenPatches ps
             rems = filter eqRemEFile ps'
             cres = filter eqCreEFile ps'
             chs  = filter (\p -> not (or [(eqRemEFile p),(eqCreEFile p)])) ps
         in  Atomic (cres ++ sortBy sortChs chs ++ rems)
   where flattenPatches :: [Patch] -> [Patch]
         flattenPatches [] = []
         flattenPatches ((Atomic ps):ps') = ps ++ flattenPatches ps'
         flattenPatches (p:ps') = p : flattenPatches ps'
         eqRemEFile (AtPath _ RemoveEmptyFile) = True
         eqRemEFile _ = False
         eqCreEFile (AtPath _ CreateEmptyFile) = True
         eqCreEFile _ = False
         sortChs :: Patch -> Patch -> Ordering
         sortChs (AtPath p1 (ChangeHunk o1 _ _)) (AtPath p2 (ChangeHunk o2 _ _)) =
            case compare p1 p2 of
               EQ -> compare o2 o1 --Sort acesending
               otherwise  -> otherwise
         sortChs _ _ = error "This can't happen"

mergeParallelPatches :: Patch -> Patch -> Patch
mergeParallelPatches p1 p2 = error "Not written yet"

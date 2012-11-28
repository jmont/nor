module Patch where
import Data.Algorithm.Diff
import Data.List

type Edit t = (DI, t)
type Path = String

data Patch = Patch Path PatchAction deriving (Show)
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
         sortChs :: Patch -> Patch -> Ordering
         sortChs (Patch p1 (ChangeHunk o1 _ _)) (Patch p2 (ChangeHunk o2 _ _)) =
            case compare p1 p2 of
               EQ -> compare o2 o1 --Sort acesending
               otherwise  -> otherwise
         sortChs _ _ = error "This can't happen"

mergeParallelPatches :: [Patch] -> [Patch] -> [Patch]
mergeParallelPatches p1 p2 = error "Not written yet"

module Patch where
import Data.Algorithm.Diff

type Edit = (DI,String)
type Path = String

data Patch = AtPath Path PatchAction
           | Atomic [Patch] deriving (Show)

data PatchAction = RemoveEmptyFile
                 | CreateEmptyFile
                 | ChangeHunk { offset :: Int -- Starting Line Number
                              , old :: [String] -- List of old lines
                              , new :: [String] -- list of new lines
                              } deriving (Show)

applyEdits :: [Edit] -> [String] -> Maybe [String]
applyEdits es strs = sequence (aE es strs)
   where aE ((B,str1):es) (str2:strs) =
            if (str1==str2) then Just str2 : aE es strs else [Nothing]
         aE ((F,str1):es) (str2:strs) =
            if (str1==str2) then aE es strs else [Nothing]
         aE ((S,str1):es) strs = Just str1 : aE es strs
         aE [] [] = []
         aE _  [] = [Nothing]

editsToPatch :: [Edit] -> Path -> Patch
editsToPatch es p = Atomic $ map (AtPath p) (editsToChangeHunks es)

editsToChangeHunks :: [Edit] -> [PatchAction]
editsToChangeHunks es = eTCH es 0
   where eqB = ((==) B . fst)
         neqB = ((/=) B . fst)
         eqF = ((==) F . fst)
         neqF = ((/=) F . fst)
         eTCH es lineNum =
            let (keeps, rest) = span eqB es
                (changes, rest'') = span neqB rest
                deletes = map snd (filter eqF changes)
                adds = map snd (filter neqF changes)
                ch = ChangeHunk (lineNum + length keeps) deletes adds
             in if (length adds + length deletes) == 0
                then []
                else ch : eTCH rest'' (offset ch + length adds - length deletes)

mergePatches :: Patch -> Patch -> Patch
mergePatches p1 p2 = error "Not written yet"

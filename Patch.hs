module Patch where

import Data.Algorithm.Diff

type Edit = (DI,String)
type Path = String

data Patch = AtPath Path PatchAction
             | Atomic [Patch]

data PatchAction = RemoveEmptyFile
                 | CreateEmptyFile
                 | ChangeHunk { offset :: Int -- Starting Line Number
                              , old :: [String] -- List of old lines
                              , new :: [String] -- list of new lines
                              }

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
editsToPatch es p = Atomic $ map (AtPath p) (eTP es 0)
   where eTP es lineNum = 
          let keeps = takeWhile eqB es
              rest = dropWhile eqB es
              changes = takeWhile eqB rest
              deletes = map snd (filter eqF changes)
              adds = map snd (filter neqB changes)
              ch = ChangeHunk (lineNum + length keeps) deletes adds
          in ch : eTP es (offset ch + length adds - length deletes)
         eqB = ((==) B . fst)
         eqF = ((==) F . fst)
         neqB = ((/=) B . fst)

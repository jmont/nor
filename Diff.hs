module Diff where
import Data.Algorithm.Diff

--d1s and d2s are diffs from a common ancestor
combinediff :: Eq t => [(DI, t)] -> [(DI, t)] -> [(DI,t)]
combinediff [] d2s = d2s
combinediff d1s [] = d1s
combinediff (d1@(S, t1):d1s) (d2@(S, t2):d2s) =
    if t1 == t2
        then d1:combinediff d1s d2s -- add same only once
        else d1:combinediff d1s (d2:d2s)
combinediff (d1@(S, t1):d1s) d2s = d1:combinediff d1s d2s --added so add
combinediff d1s (d2@(S, t2):d2s) = d2:combinediff d1s d2s --added so add
combinediff ((bof1, t1):d1s) ((bof2, t2):d2s) =
    if t1 == t2
        then case (bof1, bof2) of
            (B,B) -> (B,t1):combinediff d1s d2s -- both kept so keep
            _ -> (F,t1):combinediff d1s d2s -- >at least one removed so remove
        else error "bad"

createFromDiff :: [(DI,t)] -> [t]
createFromDiff diffs = map snd (filter (\(x,_) -> x/= F) diffs)

--Algebraic laws
--f1 == createfromdiff (getdiff 0 f1)

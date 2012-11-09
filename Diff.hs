import Data.Algorithm.Diff

f0 = ["cat", "dog"]
f1 = ["cat", "me", "cat", "hello"]
f2 = ["cat", "dog", "matt", "JC"]

d01 = getDiff f0 f1
d02 = getDiff f0 f2

--d1s and d2s are diffs from a common ancestor
combinediff :: Eq t => [(DI, t)] -> [(DI, t)] -> [t]
combinediff [] d2s = map snd d2s
combinediff d1s [] = map snd d1s
combinediff ((S, t1):d1s) d2s = t1:combinediff d1s d2s --added so add
combinediff d1s ((S, t2):d2s) = t2:combinediff d1s d2s --added so add
combinediff ((bof1, t1):d1s) ((bof2, t2):d2s) =
    if t1 == t2
        then case (bof1, bof2) of
            (B,B) -> t1:combinediff d1s d2s -- both kept so keep
            _ -> combinediff d1s d2s -- >1 removed so remove
        else error "bad"

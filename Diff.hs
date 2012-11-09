import Data.Algorithm.Diff

a0 = ["a"]
a1 = ["c", "b"]
a2 = ["c", "c", "c"]

da01 = getDiff a0 a1
da02 = getDiff a0 a2

b0 = ["a", "b", "c"]
b1 = ["a", "b", "d"]
b2 = ["a", "b", "e"]

db01 = getDiff b0 b1
db02 = getDiff b0 b2

c0 = ["a", "b", "c"]
c1 = ["b", "d"]
c2 = ["b", "e"]

dc01 = getDiff c0 c1
dc02 = getDiff c0 c2

f0 = ["cat", "dog"]
f1 = ["cat", "me", "cat", "hello"]
f2 = ["cat", "dog", "matt", "JC"]

df01 = getDiff f0 f1
df02 = getDiff f0 f2

g0 = ["matt","me"]
g1 = ["hey","me"]
g2 = ["bob","matt","hello","me"]

dg01 = getDiff g0 g1
dg02 = getDiff g0 g2

--d1s and d2s are diffs from a common ancestor
combinediff :: Eq t => [(DI, t)] -> [(DI, t)] -> [t]
combinediff [] d2s = map snd d2s
combinediff d1s [] = map snd d1s
combinediff ((S, t1):d1s) (d2@(S, t2):d2s) =
    if t1 == t2
        then t1:combinediff d1s d2s -- add same only once
        else t1:combinediff d1s (d2:d2s)
combinediff ((S, t1):d1s) d2s = t1:combinediff d1s d2s --added so add
combinediff d1s ((S, t2):d2s) = t2:combinediff d1s d2s --added so add
combinediff ((bof1, t1):d1s) ((bof2, t2):d2s) =
    if t1 == t2
        then case (bof1, bof2) of
            (B,B) -> t1:combinediff d1s d2s -- both kept so keep
            _ -> combinediff d1s d2s -- >1 removed so remove
        else error "bad"

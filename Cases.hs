module Cases where
--import Nor
import Rebase
import Core
import ObjectStore
import qualified Data.Set as Set
import Data.Algorithm.Diff
import Patch
import Data.List

--Diff Stuff
a0 = ["a"]
a1 = ["c", "b"]
a2 = ["c", "c", "c"]
--cbcc
da01 = getDiff a0 a1
da02 = getDiff a0 a2

b0 = ["a", "b", "c"]
b1 = ["a", "b", "d"]
b2 = ["a", "b", "e"]
--abde
db01 = getDiff b0 b1
db02 = getDiff b0 b2

c0 = ["a", "b", "c"]
c1 = ["b", "d"]
c2 = ["b", "e"]
--bde
dc01 = getDiff c0 c1
dc02 = getDiff c0 c2

f0 = ["cat", "dog"]
f1 = ["cat", "me", "cat", "hello"]
f2 = ["cat", "dog", "matt", "JC"]
--cat me cat hello matt jc
df01 = getDiff f0 f1
df02 = getDiff f0 f2

g0 = ["matt","me"]
g1 = ["hey","me"]
g2 = ["bob","matt","hello","me"]
--hey bob hello me
dg01 = getDiff g0 g1
dg02 = getDiff g0 g2

h0 = ["a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z"]
h1 = ["A","b","C","d","E","f","G","h","I","j","K","l","M","n","O","p","Q","r","S","t","U","v","W","x","Y","z"]
h2 = ["a","B","c","D","e","F","g","H","i","J","k","L","m","N","o","P","q","R","s","T","u","V","w","X","y","Z"]
--ABCDEFGHIJKLMNOPQRSTUVWXYZ
dh01 = getDiff h0 h1
dh02 = getDiff h0 h2

i0 = ["a","b","c","d","e"]
i1 = ["a","b","b2","c","d","d2","e"]
i2 = ["a","c","e"]
i3 = ["b","d"]
di01 = getDiff i0 i1
di02 = getDiff i0 i2
di03 = getDiff i0 i3

j0 = ["x","a","b","c","d","e","f","g","h","y"]
j1 = ["x","0","1","2","d","6","7","8","h","y"]
j2 = ["x","a","b","3","4","5","f","9","10","y"]
j01 = editsToPatch (getEdits j0 j1) "test"
j02 = editsToPatch (getEdits j0 j2) "test"
(jnoConfs,jconfs) = j01 >||< j02

l0 = ["a","b","c"]
l1 = ["x","a","d","c"]
l2 = ["a","d","c"]

m1 = words "a b c"
m2 = words "a b c e"
m3 = words "q b c e"
m4 = words "d b c"
m5 = words "m n b c"
p13 = editsToPatch (getEdits m1 m3) "test"
p14 = editsToPatch (getEdits m1 m4) "test"
p15 = editsToPatch (getEdits m1 m5) "test"
(pnoConfs,pconfs) = p13 >||< p14

n0 = words "a b c d e f g h"
n1 = words "a 0 2 d 4 6 8 h"
n2 = words "a b 1 3 5 f 7 h"
n01 = editsToPatch (getEdits n0 n1) "test"
n02 = editsToPatch (getEdits n0 n2) "test"
(nnoConfs,nconfs) = n01 >||< n02

p0 = ["",""]
p1 = []
p2 = ["a",""]

-- Testing new rebase with an example that conflicts
q0 = words "a b c"
q1 = words "m n b c"
q2 = words "m n b c e"
q3 = words "a b d"
q4 = words "q b f"
qFoundation = editsToPatch (getEdits q0 q2) "test"
q03 = editsToPatch (getEdits q0 q3) "test"
q34 = editsToPatch (getEdits q3 q4) "test"
qd0 = DataCommit (Just (DataCommit Nothing Set.empty)) (Set.singleton (File "test" q0))
-- Begin Branch A
qd1 = DataCommit (Just qd0) (Set.singleton (File "test" q1))
qd2 = DataCommit (Just qd1) (Set.singleton (File "test" q2))
-- Begin Branch B
qd3 = DataCommit (Just qd0) (Set.singleton (File "test" q3))
qd4 = DataCommit (Just qd3) (Set.singleton (File "test" q4))
--(dc@(DataCommit _ fileSet),Fail (CP confs noConfs,toRs)) = replayConf qd2 [qd3,qd4]
--noConfsSeq = sequenceParallelPatches noConfs
--invertedPatches = concatMap (\(Conflict _ p1s) -> map invert p1s) confs
--conflictPatches = map conflictAsPatch confs
--adjInvertedPs = invertedPatches `adjustedByPPatch` noConfs
--adjConflictPs = conflictPatches `adjustedByPPatch` noConfs
--adjInvertedPsSeq = sequenceParallelPatches adjInvertedPs
--adjConflictPsSeq = sequenceParallelPatches adjConflictPs
--newFiles = ((applyPatches adjConflictPsSeq) . (applyPatches adjInvertedPsSeq) . (applyPatches noConfsSeq)) (Set.toList fileSet)

--Example where the way the patches are created affects if there is conflicts
--originally d1 -> d4 was done in one patch which wouldn't conflict, but the
--algorithm now creates multiple small patches, leading to a conflict
d0 = DataCommit Nothing Set.empty
d1 = DataCommit (Just d0) (Set.singleton (File "P" ["C","R","W","J","G","J","Y","G"]))
-- Begin Branch A
d2 = DataCommit (Just d1) (Set.singleton (File "P" ["C","R","W","J","G","J","Y","J","Y","B","y","m","k","b","s","j","u","o","G","C","u","s","P","Z","U","b","c","X","Z","d","o"]))
d3 = DataCommit (Just d2) (Set.singleton (File "P" ["C","R","W","J","s","v","I","G","V","J","Y","J","Y","B","y","m","k","b","s","j","u","o","G","C","u","s","P","Z","U","b","c","X","Z","d","o"]))
-- Begin Branch B
d4 = DataCommit (Just d1) (Set.singleton (File "P" ["C","T","d","z","L","f","O","q","N","S","G","w","G","F","n","v","O","R","l","R","u","B","A","P","J","g","H","I","H","N","p","F","J","G","J","Y","G"]))

-- New failure case where e2 and e4 dont conflict in their changes but e2 and e3
-- do, causing the property rebaseEq to fail
e0 = DataCommit Nothing Set.empty
e1 = DataCommit (Just e0) (Set.singleton (File "S" ["c","H","R","F","E","p","n","q"]))
-- Begin Branch A
e2 = DataCommit (Just e1) (Set.singleton (File "S" ["c","H","R","F","E","p","n","V","s","c","A","e","k","A","L","V","g"]))
-- Begin Branch B
e3 = DataCommit (Just e1) (Set.singleton (File "S" ["c","H","R","W","l","A","W","v","U","Y","i","y","W","c","x","x","M","I","N","j","V","k","q","l","P","A","x","s","S","A","P","y","b","f","b","B","Z","Q","E","O","f","H","b","I","K","L","W","k","L","n","a","X","X","f","K","C","A","V","K","L","i","M","G","L","m","w","t","Y","A","v","t","b","y","p","D","o","S","h","G","l","I","R","P","t","i","I","d","x","R","g","u","o","c","I","e","E","X","A","S","z","W","u","g","A","h","G","Q","I","w","X","X","I","m","w","U","i","V","l","R","Q","b","m","w","x","n","X","E","p","n","q"]))
e4 = DataCommit (Just e3) (Set.singleton (File "S" ["q","S","R","u","R","O","T","I","v","V","j","E","k","T","H","r","p","A","v","H","v","M","e","f","G","S","w","x","w","O","k","R","W","l","A","W","v","U","Y","i","y","W","c","x","x","M","I","N","j","V","k","q","l","P","A","x","s","S","A","P","y","b","f","b","B","Z","Q","E","O","f","H","b","I","K","L","W","k","L","n","a","X","X","f","K","C","A","V","K","L","i","M","G","L","m","w","t","Y","A","v","t","b","y","p","D","o","S","h","G","l","I","R","P","t","i","I","d","x","R","g","u","o","c","I","e","E","X","A","S","z","W","u","g","A","h","G","Q","I","w","X","X","I","m","w","U","i","V","l","R","Q","b","m","w","x","n","X","E","p","n","q"]))

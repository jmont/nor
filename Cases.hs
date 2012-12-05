module Cases where
import Nor
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
(noConfs,confs) = p13 >||< p14

n0 = words "a b c d e f g h"
n1 = words "a 0 2 d 4 6 8 h"
n2 = words "a b 1 3 5 f 7 h"
n01 = editsToPatch (getEdits n0 n1) "test"
n02 = editsToPatch (getEdits n0 n2) "test"
(nnoConfs,nconfs) = n01 >||< n02

p0 = ["a", "", "a", "", "a"]
p1 = []
p2 = ["",""]
p01 = editsToPatch (getEdits p0 p1) "test"
p02 = editsToPatch (getEdits p0 p2) "test"
--(onoConfs,oconfs) = p01 >||< p02
--oconfCHs = map (\(AP _ c) -> c) oconfs
--Conflict och1s och2s = head oconfCHs
--((ch1:ch1s), (ch2:ch2s)) =( (sort och1s),(sort och2s))

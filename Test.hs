module Test where
import Nor
import ObjectStore
import qualified Data.Set as Set
import Data.Algorithm.Diff
import Patch

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
h = ["A","b","C","d","E","f","G","h","I","j","K","l","M","n","O","p","Q","r","S","t","U","v","W","x","Y","z"]
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
dj01 = getDiff j0 j1
dj02 = getDiff j0 j2
------------------------------------------------------------------------------
--Demo of how to use WithObjects
file1 = File "test1" ["hello"]
file2 = File "test2" ["bye"]
core = (Set.empty, mkEmptyOS)
withF1 = addHashableA file1
withF2 = addHashableA file2
withF12 = withF1 >> withF2
withF12' = addHashableAs [file1,file2]
withC = createCommit withF12 Nothing
withC' = createCommit withF12' Nothing
core'  = addCommit withC core
core'' = addCommit withC' core

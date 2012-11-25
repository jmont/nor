-- Oct 17, 2012
type Blob = String
type Branch = (String, Commit)
type Repo = [Branch]
type Index = Commit
type World = (Repo, Index)

data Tree a = Leaf a | Node [Tree a]
data Commit = Empty | C (Commit, Tree Blob)

init :: Repo
init = branch [] C (Nothing, Leaf "") "master"

commit :: Commit -> Tree Blob -> Commit
commit parent workTree = CNode (parent, workTree)

branch :: String -> World -> World
branch name (repo, index) = ((name, parent):repo, index)

branch :: Repo -> Commit -> String -> Repo
branch repo (CNode (parent, tree))@par name = Repo ++ [CNode(par, tree)]

rebaseL :: Branch -> Branch -> Branch
rebaseL c1@(n1, par1) c2@(n2, par2) = 
    let comPar = commonAncestor par1 par2
        f src@(n,p) dst stop 
            | stop == p = (n, dst)
            | otherwise = (f p dst stop, mrg src dst) 
     in f c1 c2 comPar

rebase :: Repo -> String -> String -> Repo
rebase repo src dst = liftB rebaseL src dst repo 

liftB :: (Branch -> Branch -> Branch) -> String -> String -> Repo -> Repo
liftB f n1 n2 repo = 
    let Just p1 = find ((== n1) . fst) repo
        Just p2 = find ((== n2) . fst) repo
     in f p1 p2 : filter (conj ((/=) n1) ((/=) n2)) repo


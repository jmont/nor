module Nor where

import Control.Monad

commit :: [String] -> IO ()
commit _ = putStrLn "commit"

---------------------------------

data File = File { path :: String -- Unix filepath: "/foo/bar/baz"
                 , contents :: [String] -- Simple representation for now
                 } deriving (Show)
type Hash = String -- Cryptographic hash
type HashDict = [(Hash, File)]

mkHashDict = []

addHash :: HashDict -> Hash -> File -> HashDict
addHash hs h f = (h,f):hs

findFile :: HashDict -> Hash -> Maybe File
findFile hd hash = 
    foldl (\res (h,f) -> 
        if h == hash then Just f else res) Nothing hd

getHash :: HashDict -> File -> Maybe Hash
getHash hd file = 
    foldl (\res (h,f) ->
        if path file == path f then Just h else res) Nothing hd

data Commit = Commit { parent :: Maybe Commit -- Initial commit has nothing
                     , hashes :: [Hash] -- Hashes of all files at given time
                     , cid :: Int -- Unique identifier
                     } deriving (Show)
type Repo = [Commit]

-- list of all commits, hash->file, head commit, commitCount
type World = (Repo, HashDict, Maybe Commit, Int)
--newtype ShowWorld a = ShowWorld { getWorld :: a }
--instance Show (ShowWorld a)  where
--    show sw = printWorld $ getWorld sw
--printWorld w@(r, _, headC, _) = show r

-- An empty world
init :: World
init = ([], mkHashDict, Nothing, 0)

-- Commits changes, represented in the hash/file tuples, of all files at the
-- current time; add new commit to repository, add new unique hash/file tuples
-- to the hashDict, update HEAD commit, and increase the commit count.
-- TODO: what about committing a nonempty repo with no changes?
lowCommit :: World -> [(Hash, File)] -> World
lowCommit world@(repo, hashdict, headC, cCount) [] = world
lowCommit (repo, hashdict, headC, cCount) hfs =
    let hashdict' = foldl (\hashdict (h, f) ->
                        case findFile hashdict h 
                            of Nothing -> addHash hashdict h f
                               otherwise -> hashdict) hashdict hfs
        newC = Commit headC (map fst hfs) cCount
        in (newC:repo, hashdict', Just newC, cCount + 1)

-- In the world, set HEAD commit to the commit referenced by id if it exists.
commitById :: World -> Int -> Maybe Commit
commitById (repo, hashdict, headC, cCount) id =
    foldl (\mc c@(Commit pc hashes cid) ->
                if id == cid then Just c
                             else mc) Nothing repo

medCheckout :: World -> Int -> Maybe (World, [File])
medCheckout w@(r, hashdict, _, cc) id = do
    headC' <- commitById w id
    files <- mapM (findFile hashdict) (hashes headC')
    return ((r, hashdict, Just headC', cc), files)


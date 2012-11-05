module Nor where

import Control.Monad

commit :: [String] -> IO ()
commit _ = putStrLn "commit"

---------------------------------

data File = File { path :: String -- Unix filepath: "/foo/bar/baz"
                 , contents :: [String] -- Simple representation for now
                 } deriving (Show)
type Hash = String -- Cryptographic hash


mkHashDict :: Hash -> Maybe File
mkHashDict = \_ -> Nothing

addHash :: (Hash -> Maybe File) -> Hash -> File -> (Hash -> Maybe File)
addHash hashdict h f = (\x -> if x == h then Just f
                                  else hashdict h)

data Commit = Commit { parent :: Maybe Commit -- Initial commit has nothing
                     , hashes :: [Hash] -- Hashes of all files at given time
                     , cid :: Int -- Unique identifier
                     } deriving (Show)
type Repo = [Commit]

-- list of all commits, hash->file, head commit, commitCount
type World = (Repo, Hash -> Maybe File, Maybe Commit, Int)
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
                        case hashdict h of Nothing -> addHash hashdict h f
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
    files <- mapM hashdict (hashes headC')
    return ((r, hashdict, Just headC', cc), files)

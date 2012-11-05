module Nor where

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
addHash hd h f = (\x -> if x == h then Just f
                                  else hd h)

data Commit = Commit { parent :: Maybe Commit -- Initial commit has nothing
                     , newHs :: [Hash] -- Hashes of all files at given time
                     , cid :: Int -- Unique identifier
                     } deriving (Show)
type Repo = [Commit]

-- list of all commits, hash->file, head commit, commitCount
type World = (Repo, Hash -> Maybe File, Maybe Commit, Int)

-- An empty world
init :: World
init = ([], mkHashDict, Nothing, 0)

-- Commits changes, represented in the hash/file tuples, of all files at the
-- current time; add new commit to repository, add new unique hash/file tuples
-- to the hashDict, update HEAD commit, and increase the commit count.
-- TODO: what about committing a nonempty repo with no changes?
lowCommit :: World -> [(Hash, File)] -> World
lowCommit world@(repo, hd, headC, cCount) [] = world
lowCommit (repo, hd, headC, cCount) hfs =
    let hd' = foldl (\hd (h, f) ->
                        case hd h of Nothing -> addHash hd h f
                                     otherwise -> hd) hd hfs
        newC = Commit headC (map fst hfs) cCount
        in (newC:repo, hd', Just newC, cCount + 1)

-- In the world, set HEAD commit to the commit referenced by id if it exists.
lowCheckout :: World -> Int -> Maybe World
lowCheckout (repo, hd, headC, cCount) id =
    foldl (\mw c@(Commit pc hs cid) ->
                if id == cid then Just (repo, hd, Just c, cCount)
                             else mw) Nothing repo

module Nor where

commit :: [String] -> IO ()
commit _ = putStrLn "commit"

---------------------------------

data File = File { path :: String -- Unix filepath: "/foo/bar/baz"
                 , lines :: [String] -- Simple representation for now
                 } deriving (Show)
type Hash = String -- Cryptographic hash


mkHashDict :: Hash -> Maybe File
mkHashDict = \_ -> Nothing

addHash :: (Hash -> Maybe File) -> Hash -> File -> (Hash -> Maybe File)
addHash hd h f = (\x -> if x == h then Just f
                                  else hd h)

data Commit = Commit { parent :: Maybe Commit -- Initial commit has nothing
                     , newHs :: [Hash] -- Hashes of files which changed
                     , cid :: Int -- Unique identifier
                     } deriving (Show)
type Repo = [Commit]

-- head commit, hash->file, head commit, commitCount
type World = (Repo, Hash -> Maybe File, Maybe Commit, Int)

-- An empty world
init :: World
init = ([], mkHashDict, Nothing, 0)

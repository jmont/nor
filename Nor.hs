module Nor where
import Control.Monad
import qualified Data.Map as Map
import Crypto.Hash.SHA1 (hashlazy, hash)
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString as Strict
---------------------------------

type Hash = Strict.ByteString -- Cryptographic hash

class Hashable a where
   getHash :: a -> Hash
instance Hashable Strict.ByteString where --Hashable Hash
   getHash h = h
instance Hashable a => Hashable [a] where
   getHash as = hash (Strict.concat (map getHash as))

data File = File { path :: String -- Unix filepath: "/foo/bar/baz"
                 , contents :: [Lazy.ByteString] -- Simple representation for now
                 } deriving (Show)

instance Hashable File where
   getHash f = hashlazy (Lazy.concat (contents f)) -- Doesn't include path atm

type HashEntry = (Hash, File)
type HashDict = Map.Map Hash File

mkHashDict = Map.empty

addHash :: HashDict -> Hash -> File -> HashDict
addHash hd h f = Map.insert h f hd

findFile :: HashDict -> Hash -> Maybe File
findFile hd hash =
    Map.lookup hash hd

getFiles :: HashDict -> [File]
getFiles = Map.elems

data Commit = Commit { parent :: Maybe Commit -- Initial commit has nothing
                     , hashes :: [Hash] -- Hashes of all files at given time
                     , cid :: Hash
                     } deriving (Show)

type Repo = [Commit]

-- list of all commits, hash->file, head commit, commitCount
type World = (Repo, HashDict, Commit)
--newtype ShowWorld a = ShowWorld { getWorld :: a }
--instance Show (ShowWorld a)  where
--    show sw = printWorld $ getWorld sw
--printWorld w@(r, _, headC, _) = show r

-- An empty world
init :: World
init = ([], mkHashDict, Commit Nothing [] (getHash ([]::[Hash])))



-- Commits changes, represented in the hash/file tuples, of all files at the
-- current time; add new commit to repository, add new unique hash/file tuples
-- to the hashDict, update HEAD commit, and increase the commit count.
-- TODO: what about committing a nonempty repo with no changes?
lowCommit :: World -> [File] -> World
lowCommit world@(repo, hashdict, headC) [] = world
lowCommit (repo, hashdict, headC) fs =
    let  hs = map getHash fs
         hashdict' = foldl (\hashdict (h,f) ->
                        case findFile hashdict h
                            of Nothing -> addHash hashdict h f
                               otherwise -> hashdict) hashdict (zip hs fs)
         newC = Commit (Just headC) hs (getHash hs)
    in (newC:repo, hashdict', newC)

-- In the world, set HEAD commit to the commit referenced by id if it exists.
commitById :: World -> Hash -> Maybe Commit
commitById (repo, hashdict, headC) id =
    foldl (\mc c@(Commit pc hashes cid) ->
                if id == cid then Just c
                             else mc) Nothing repo

medCheckout :: World -> Hash -> Maybe (World, [File])
medCheckout w@(r, hashdict, _) id = do
    headC' <- commitById w id
    files <- mapM (findFile hashdict) (hashes headC')
    return ((r, hashdict, headC'), files)


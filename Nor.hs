module Nor where
import Control.Monad
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List
import Diff
import Data.Algorithm.Diff
import qualified Data.Set as Set
import ObjectStore
import Crypto.Hash.SHA1 (hashlazy, hash)
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString as Strict
import Data.Serialize 
---------------------------------

data File = File { path :: String -- Unix filepath: "/foo/bar/baz"
                 , contents :: [Lazy.ByteString] -- Simple representation for now
                 } deriving (Show)

instance Serialize File where
    put f = put ((encodeLazy (path f)) : (contents f))
    get = getListOf get >>= mapM Data.Serialize.getLazyByteString >>= 
            (\lbsList -> return (File "" lbsList))
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
instance Ord Commit where
   compare c1 c2 = compare (cid c1) (cid c2)
instance Eq Commit where
   (==) c1 c2 = (cid c1) == (cid c2)

-- list of all commits, hash->file, head commit, commitCount
type Core = (Set.Set Commit, HashDict)
type World = (Core, Commit)
--newtype ShowWorld a = ShowWorld { getWorld :: a }
--instance Show (ShowWorld a)  where
--    show sw = printWorld $ getWorld sw
--printWorld w@(r, _, headC, _) = show r

-- An empty world
--init :: World
--init = let initC = Commit Nothing [] (getHash ([]::[Hash]))
--       in ([initC], mkHashDict, initC)



-- Commits changes, represented in the hash/file tuples, of all files at the
-- current time; add new commit to repository, add new unique hash/file tuples
-- to the hashDict, update HEAD commit, and increase the commit count.
-- TODO: what about committing a nonempty repo with no changes?
--lowCommit :: World -> [File] -> World
--lowCommit world@(repo, hashdict, headC) [] = world
--lowCommit (repo, hashdict, headC) fs =
--    let  hs = map getHash fs
--         hashdict' = foldl (\hashdict (h,f) ->
--                        case findFile hashdict h
--                            of Nothing -> addHash hashdict h f
--                               otherwise -> hashdict) hashdict (zip hs fs)
--         newC = Commit (Just headC) hs (getHash hs)
--    in (newC:repo, hashdict', newC)
--
---- In the world, set HEAD commit to the commit referenced by id if it exists.
--commitById :: World -> Hash -> Maybe Commit
--commitById (repo, hashdict, headC) id =
--    foldl (\mc c@(Commit pc hashes cid) ->
--                if id == cid then Just c
--                             else mc) Nothing repo
--
--medCheckout :: World -> Hash -> Maybe (World, [File])
--medCheckout w@(r, hashdict, _) id = do
--    headC' <- commitById w id
--    files <- mapM (findFile hashdict) (hashes headC')
--    return ((r, hashdict, headC'), files)
--
--getLca :: Commit -> Commit -> Maybe Commit
--getLca  ca cb =
--   let  withSet (Commit Nothing _ _) (Commit Nothing _ _) set = Nothing
--        withSet (Commit (Just p1) _ _) c2 set =
--         if Set.member p1 set
--         then Just p1
--         else withSet p1 c2 set
--        withSet c1 c2 set = withSet c2 c1 set
--   in withSet ca cb Set.empty
--

module Core
  ( File(..)
  , Commit(..)
  , Core
  , CoreReader(..), CoreExtender(..)
  , initCore
  , CR(..)
  , CX(..)
  , getFilesForCom
  , commitById'
  )
where

import Control.Applicative
import qualified Data.Maybe as Maybe
import Data.Serialize
import qualified Data.Set as Set
import Crypto.Hash.SHA1 (hash)
import qualified Data.ByteString as BS

import ObjectStore

data CR a = CR { rc :: Core -> a }
data CX a = CX { xc :: Core -> (a, Core) }

-- list of all commits, hash->file, head commit, commitCount
type Core = (Set.Set (Commit Hash), ObjectStore File)

data File = File { path :: String -- Unix filepath: "/foo/bar/baz"
                 , contents :: [String] -- Simple representation for now
                 } deriving (Show,Eq)

data Commit a = Commit { parent :: Maybe Hash -- Initial commit has nothing
                       , cContents :: [a] -- Associated as with commit
                       , cid :: Hash
                       } deriving (Show)

class Monad m => CoreReader m where
    readCore :: m Core

class CoreReader m => CoreExtender m where
    addCommit' :: [File] -> Hash -> m (Commit Hash)

instance Monad CR where
  return a = CR $ const a
  (CR r) >>= k = CR $ \core -> rc (k (r core)) core

instance Monad CX where
  return a = CX $ \c -> (a, c)
  (CX m) >>= k = CX $ \c -> let (b, c') = m c in xc (k b) c'

instance CoreReader CR where
  readCore = CR $ id

instance CoreReader CX where
  readCore = CX $ \c -> (c, c)

instance CoreExtender CX where
  addCommit' fs pcid = CX $ \(cs, os) ->
    let (hs, os') = addObjects os fs
        c' = Commit (Just pcid) hs $ mkCommitHash (pcid:hs)
    in (c', (Set.insert c' cs, os'))
    where mkCommitHash :: [Hash] -> Hash
          mkCommitHash = Hash . hash . BS.concat . map getHash

instance Serialize File where
    put (File p c) = put p >> put c
    get = File <$> get <*> get

instance Ord (Commit a) where
   compare c1 c2 = compare (cid c1) (cid c2)
instance Eq (Commit a) where
   c1 == c2 = cid c1 == cid c2
instance Serialize a => Serialize (Commit a) where
    put (Commit pid hs id) = put pid >> put hs >> put id
    get = Commit <$> get <*> get <*> get

getFilesForCom :: CoreReader m => Commit Hash -> m [File]
getFilesForCom com = do
  (_,os) <- readCore
  return $ Maybe.mapMaybe (getObject os) (cContents com)

commitById' :: CoreReader m => Hash -> m (Commit Hash)
commitById' id = readCore >>= (\(commitSet,_) -> return
    (foldl (\z c@(Commit _ _ cid) -> if id == cid then c else z)
           (error "Commit not found") (Set.elems commitSet)))

-- An empty Core
initCore :: Core
initCore = let initC = Commit Nothing [] $ Hash (hash (encode ""))
           in (Set.singleton initC, mkEmptyOS)

module Core 
  ( File(..)
  , Commit(..)
  , Core
  , CoreReader(..), CoreExtender(..)
  ) 
where

import Control.Applicative
import Data.Serialize
import qualified Data.Set as Set

import ObjectStore

class Monad m => CoreReader m where
    readCore :: m Core

class CoreReader m => CoreExtender m where
    addFile   :: File   -> m Hash
    addCommit' :: Commit -> m ()


_createRepo :: IO () -- writes an initial core
_createRepo = unimp

_updateRepo :: CX a -> IO a               
_updateRepo = unimp

_loadRepo :: CR a -> IO a    
_loadRepo = unimp

data CR a = CR { rc :: Core -> a }
data CX a = CX { xc :: Core -> (a, Core) }

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
  addFile file = CX $ \core -> (hash_of_new_file file core, new_core file core)
   where hash_of_new_file = unimp
         new_core = unimp
        
  addCommit' c = CX $ \core -> ((), core_with_commit core c)
   where core_with_commit = unimp
        



data File = File { path :: String -- Unix filepath: "/foo/bar/baz"
                 , contents :: [String] -- Simple representation for now
                 } deriving (Show,Eq)

instance Serialize File where
    put (File p c) = put p >> put c
    get = File <$> get <*> get

data Commit = Commit { parent :: Maybe Hash -- Initial commit has nothing
                     , hashes :: [Hash] -- Hashes of all files at given time
                     , cid :: Hash
                     } deriving (Show)

instance Ord Commit where
   compare c1 c2 = compare (cid c1) (cid c2)
instance Eq Commit where
   c1 == c2 = cid c1 == cid c2
instance Serialize Commit where
    put (Commit pid hs id) = put pid >> put hs >> put id
    get = Commit <$> get <*> get <*> get

-- list of all commits, hash->file, head commit, commitCount
type Core = (Set.Set Commit, ObjectStore File)


unimp :: a
unimp = error "not impelmented"

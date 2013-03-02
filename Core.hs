module Core
  ( File(..)
  , Commit(..)
  , Core
  , CoreReader(..), CoreExtender(..)
  , initCore
  )
where

import Control.Applicative
import Data.Serialize
import qualified Data.Set as Set
import Crypto.Hash.SHA1 (hash)

import ObjectStore

data CR a = CR { rc :: Core -> a }
data CX a = CX { xc :: Core -> (a, Core) }

-- list of all commits, hash->file, head commit, commitCount
type Core = (Set.Set Commit, ObjectStore File)

data File = File { path :: String -- Unix filepath: "/foo/bar/baz"
                 , contents :: [String] -- Simple representation for now
                 } deriving (Show,Eq)

data Commit = Commit { parent :: Maybe Hash -- Initial commit has nothing
                     , hashes :: [Hash] -- Hashes of all files at given time
                     , cid :: Hash
                     } deriving (Show)

class Monad m => CoreReader m where
    readCore :: m Core

class CoreReader m => CoreExtender m where
    addFile   :: File   -> m Hash
    addCommit' :: Commit -> m ()

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
  addFile file = CX $ \(cs, os) ->
                            let (h, os') = addObject os file
                            in (h, (cs, os'))
  --(hash_of_new_file file core, new_core file core)
  addCommit' c = CX $ \(cs, os) -> ((), (Set.insert c cs, os)) -- TODO fix

instance Serialize File where
    put (File p c) = put p >> put c
    get = File <$> get <*> get

instance Ord Commit where
   compare c1 c2 = compare (cid c1) (cid c2)
instance Eq Commit where
   c1 == c2 = cid c1 == cid c2
instance Serialize Commit where
    put (Commit pid hs id) = put pid >> put hs >> put id
    get = Commit <$> get <*> get <*> get

-- An empty Core
initCore :: Core
initCore = let initC = Commit Nothing [] $ Hash (hash (encode ""))
           in (Set.singleton initC, mkEmptyOS)

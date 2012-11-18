module ObjectStore where
import Crypto.Hash.SHA1 (hashlazy, hash)
import Data.Serialize
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString as Strict
import qualified Data.Foldable as F
-------------------------------------------------

type Hash = Strict.ByteString
data ObjectStore a = OS { store :: Map.Map Hash a } deriving Show

mkEmptyOS :: ObjectStore a
mkEmptyOS = OS (Map.empty)

addObject :: Serialize a => ObjectStore a -> a -> (Hash, ObjectStore a)
addObject os a = let newHash = hash (encode a)
                 in (newHash, OS $ Map.insert newHash a (store os))

getObject :: ObjectStore a -> Hash -> Maybe a
getObject os h = Map.lookup h (store os)

getHashes :: ObjectStore a -> [Hash]
getHashes = Map.keys . store

instance Functor ObjectStore where
   fmap fn fa = OS $ fmap fn (store fa)
instance F.Foldable ObjectStore where
   foldMap fn fa = F.foldMap fn (store fa)

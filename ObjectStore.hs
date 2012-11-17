module ObjectStore where
import Crypto.Hash.SHA1 (hashlazy, hash)
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString as Strict
-------------------------------------------------

type Hash = Strict.ByteString -- Cryptographic hash

class Hashable a where
    getHash :: a -> Hash
instance Hashable Strict.ByteString where --Hashable Hash
    getHash h = h
instance Hashable a => Hashable [a] where
    getHash as = hash (Strict.concat (map getHash as))

data ObjectStore a = OS { store :: Map.Map Hash a }

mkEmptyOS :: ObjectStore a
mkEmptyOS = OS (Map.empty)

addObject :: Hashable a => ObjectStore a -> a -> ObjectStore a
addObject os a = OS $ Map.insert (getHash a) a (store os)

getObject :: ObjectStore a -> Hash -> Maybe a
getObject os h = Map.lookup h (store os)

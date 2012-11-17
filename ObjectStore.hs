module ObjectStore where
import Crypto.Hash.SHA1 (hashlazy, hash)
import Data.Serialize
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString as Strict
-------------------------------------------------

type Hash = Strict.ByteString
data ObjectStore a = OS { store :: Map.Map Hash a }

mkEmptyOS :: ObjectStore a
mkEmptyOS = OS (Map.empty)

addObject :: Serialize a => ObjectStore a -> a -> ObjectStore a
addObject os a = OS $ Map.insert (hash (encode a)) a (store os)

getObject :: ObjectStore a -> Hash -> Maybe a
getObject os h = Map.lookup h (store os)

module ObjectStore where
import Control.Applicative
import Crypto.Hash.SHA1 (hash)
import Data.Serialize
import Numeric (showHex, readHex)
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString as Strict
import qualified Data.Foldable as F
-------------------------------------------------

makeSizeTwo :: String -> String
makeSizeTwo i@(_:[]) = '0':i
makeSizeTwo i@(_:_:[]) = i

splitInTwos :: [a] -> [[a]]
splitInTwos (a:b:rest) = [a,b] : splitInTwos rest
splitInTwos [] = []

hexToHash :: String -> Hash
hexToHash hx = Hash $ Strict.pack . map fst
                    . concatMap readHex . splitInTwos $ hx

newtype Hash = Hash { getHash :: Strict.ByteString } deriving (Eq, Ord)

instance Show Hash where
    show = concatMap makeSizeTwo
                  . map (`showHex` "") . Strict.unpack . getHash

instance Serialize Hash where
    put (Hash h) = put h
    get = Hash <$> get

data ObjectStore a = OS { store :: Map.Map Hash a } deriving Show

instance (Serialize a) => Serialize (ObjectStore a) where
    put (OS s) = put s
    get = OS <$> get

mkEmptyOS :: ObjectStore a
mkEmptyOS = OS Map.empty

addObject :: Serialize a => ObjectStore a -> a -> (Hash, ObjectStore a)
addObject os a = let newHash = Hash $ hash (encode a)
                 in (newHash, OS $ Map.insert newHash a (store os))

--Return the object with the corresponding hash, if it exists.
getObject :: ObjectStore a -> Hash -> Maybe a
getObject os h = Map.lookup h (store os)

getObjects :: ObjectStore a -> [Hash] -> Maybe [a]
getObjects os = mapM (getObject os)

getHashes :: ObjectStore a -> [Hash]
getHashes = Map.keys . store

addObjects :: Serialize a => ObjectStore a -> [a] -> ([Hash], ObjectStore a)
addObjects os = foldr
                    (\f (hs,os) -> let (h,os') = addObject os f in (h:hs,os'))
                    ([],os)

instance Functor ObjectStore where
   fmap fn fa = OS $ fmap fn (store fa)
instance F.Foldable ObjectStore where
   foldMap fn fa = F.foldMap fn (store fa)

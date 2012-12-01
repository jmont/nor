module ObjectStore where
import Control.Applicative
import Crypto.Hash.SHA1 (hashlazy, hash)
import Data.Serialize
import Numeric (showHex, readHex)
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString as Strict
import qualified Data.Foldable as F
-------------------------------------------------

makeSizeTwo :: String -> String
makeSizeTwo i@(a:[]) = '0':i
makeSizeTwo i@(a:b:[]) = i 

splitInTwos :: [a] -> [[a]]
splitInTwos (a:b:rest) = [a,b]:(splitInTwos rest)
splitInTwos [] = []

hexToHash :: String -> Hash
hexToHash hx = Hash $ Strict.pack . map fst 
                    . concat . map readHex . splitInTwos $ hx

newtype Hash = Hash { getHash :: Strict.ByteString } deriving (Eq, Ord)
instance Show Hash where
    show = concat . map makeSizeTwo 
                  . map (flip showHex "") . Strict.unpack . getHash
instance Serialize Hash where
    put (Hash h) = put h
    get = Hash <$> get

data ObjectStore a = OS { store :: Map.Map Hash a } deriving Show

mkEmptyOS :: ObjectStore a
mkEmptyOS = OS (Map.empty)

addObject :: Serialize a => ObjectStore a -> a -> (Hash, ObjectStore a)
addObject os a = let newHash = Hash $ hash (encode a)
                 in (newHash, OS $ Map.insert newHash a (store os))

getObject :: ObjectStore a -> Hash -> Maybe a
getObject os h = Map.lookup h (store os)

getHashes :: ObjectStore a -> [Hash]
getHashes = Map.keys . store

addObjects :: Serialize a => ObjectStore a -> [a] -> ([Hash], ObjectStore a)
addObjects os as = foldr (\f (hs,os) ->
                     let (h,os') = addObject os f
                     in (h:hs,os')) ([],os) as

instance Functor ObjectStore where
   fmap fn fa = OS $ fmap fn (store fa)
instance F.Foldable ObjectStore where
   foldMap fn fa = F.foldMap fn (store fa)

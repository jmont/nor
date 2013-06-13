module Core
  ( File(..)
  , HashCommit(..)
  , DataCommit(..)
  , Core
  , CoreReader(..), CoreExtender(..)
  , initCore
  , getFilesForCom
  , commitById
  , coreToIO
  , coreToGen
  , dataCommitById
  , firstHCom
  , firstDCom
  , getLcaH
  , ancestorListH
  )
where

import Control.Applicative
import Control.Monad
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.Serialize
import qualified Data.Set as Set
import Crypto.Hash.SHA1 (hash)
import qualified Data.ByteString as BS
import Test.QuickCheck.Gen

import ObjectStore

data CR a = CR { rc :: Core -> a }
data CX a = CX { xc :: Core -> (a, Core) }

-- list of all commits, hash->file, head commit, commitCount
type Core = (Set.Set (HashCommit), ObjectStore File)

data File = File { path :: String -- Unix filepath: "/foo/bar/baz"
                 , contents :: [String] -- Simple representation for now
                 } deriving (Show,Read,Eq,Ord)

data HashCommit = HashCommit { hparent :: Maybe Hash     -- Initial commit has nothing
                             , hContents :: Set.Set Hash-- Associated as with commit
                             , cid :: Hash
                       } deriving (Show,Read)

data DataCommit a = DataCommit { dparent :: Maybe (DataCommit a)
                               , dContents :: Set.Set a -- Associated as with commit
                               } deriving (Show,Read,Ord,Eq)

class Monad m => CoreReader m where
    readCore :: m Core

class CoreReader m => CoreExtender m where
    addCommit :: DataCommit File -> m (HashCommit)

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

-- This could do a better job of returning quickly when already in the set
instance CoreExtender CX where
  addCommit (DataCommit (Just pc) fs) = CX $ \(cs, os) ->
    let(hpc,(cs',os')) = xc (addCommit pc) (cs,os)
       (hs, os'') = addObjects os' (Set.toList fs)
       pcid = cid hpc
       hashSet = Set.fromList hs
       c' = HashCommit (Just pcid) hashSet $ mkCommitHash (pcid : List.sort hs)
    in (c', (Set.insert c' cs', os''))
    where mkCommitHash :: [Hash] -> Hash
          mkCommitHash = Hash . hash . BS.concat . map getHash
  -- Might want a better check here
  addCommit (DataCommit Nothing _) = return $ firstHCom

instance Serialize File where
    put (File p c) = put p >> put c
    get = File <$> get <*> get

instance Ord HashCommit where
   compare c1 c2 = compare (cid c1) (cid c2)
instance Eq HashCommit where
   c1 == c2 = cid c1 == cid c2
instance Serialize HashCommit where
    put (HashCommit pid hs id) = put pid >> put hs >> put id
    get = HashCommit <$> get <*> get <*> get

--TODO: This function could do better error handling
getFilesForCom :: CoreReader m => HashCommit -> m [File]
getFilesForCom com = do
  (_,os) <- readCore
  return $ Maybe.mapMaybe (getObject os) (Set.toList (hContents com))

commitById :: CoreReader m => Hash -> m (HashCommit)
commitById id = readCore >>= (\(commitSet,_) -> return
    (foldl (\z c@(HashCommit _ _ cid) -> if id == cid then c else z)
           (error "Commit not found") (Set.elems commitSet)))

getLcaH :: CoreReader m => HashCommit -> HashCommit -> m (HashCommit)
getLcaH ca cb = do
   ancSetA <- liftM Set.fromList (ancestorListH ca)
   ancB <- ancestorListH cb
   return (foldr (\a z -> if Set.member a ancSetA then a else z)
      (error "No LCA") ancB)

-- youngest to oldest order
ancestorListH :: CoreReader m => HashCommit -> m [HashCommit]
ancestorListH c1@(HashCommit Nothing _ _)    = return [c1]
ancestorListH c1@(HashCommit (Just pid) _ _) = readCore >>
   liftM (c1 :) (commitById pid >>= ancestorListH)

--commitById' :: CoreReader m => Hash -> m (Maybe HashCommit)
--commitById' id = readCore >>= (\(commitSet,_) -> return
--    (foldl (\z c@(HashCommit _ _ cid) -> if id == cid then Just c else z)
--           Nothing (Set.elems commitSet)))
--
dataCommitById :: CoreReader m => Hash -> m (DataCommit File)
dataCommitById id = do
    hashCom <- commitById id
    fs <- getFilesForCom hashCom
    pc <- getParent (hparent hashCom)
    return $ DataCommit pc (Set.fromList fs)
  where getParent :: CoreReader m => Maybe Hash -> m (Maybe (DataCommit File))
        getParent Nothing = return Nothing
        getParent (Just id) = do
          com <- dataCommitById id
          return $ Just com

firstDCom :: DataCommit File
firstDCom = DataCommit Nothing Set.empty

firstHCom :: HashCommit
firstHCom = HashCommit Nothing Set.empty $ Hash (hash (encode ""))
-- An empty Core
initCore :: Core
initCore = (Set.singleton firstHCom, mkEmptyOS)

-- Lifts Core Extender into IO
coreToIO :: CX a -> Core -> IO (a,Core)
coreToIO (CX f) c = return $ f c

-- Lifts Core Extender into Gen for Quickcheck
coreToGen :: CX a -> Core -> Gen (a,Core)
coreToGen (CX f) c = return $ f c

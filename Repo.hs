{-# LANGUAGE Rank2Types #-}
module Repo
  ( Repo
  , Ephemera(..)
  , initRepo
  , RepoReader(..)
  , RepoWriter(..)
  , repoToIO
  , repoToGen
  , liftState
  , getHC
  , getToRs
  )
where
import Control.Applicative
import Control.Monad
import Data.Serialize
import qualified Data.Set as Set
import Test.QuickCheck.Gen

import ObjectStore
import Core

class CoreReader m => RepoReader m where
    readRepo :: m Repo

class (CoreExtender m, RepoReader m) => RepoWriter m where
    updateHead :: Commit Hash -> m ()
    updateToRs :: [Commit Hash] -> m ()

data RR a = RR { rr :: Repo -> a }
data RW a = RW { wr :: CoreExtender m => Ephemera -> m (a, Ephemera) }

liftState :: Monad m => m a -> b -> m (a,b)
liftState m s = m >>= (\a -> return (a,s))

instance Monad RR where
    return a = RR $ const a
    (RR r) >>= k = RR $ \repo -> rr (k (r repo)) repo

instance CoreReader RR where
    readCore = RR $ fst

instance RepoReader RR where
    readRepo = RR $ id

instance Monad RW where
    return a = RW $ \eph -> return (a, eph)
    (RW m) >>= k = RW $ \eph -> do { (b, eph') <- m eph ; wr (k b) eph' }

instance CoreReader RW where
    readCore = RW $ \eph -> readCore >>= (\core -> return (core,eph))

-- Additionaly updates the head commit
instance CoreExtender RW where
    addCommit fs pc = RW (liftState (addCommit fs pc)) >>=
                       (\com -> updateHead com >> return com)

instance RepoReader RW where
    readRepo = RW $ \eph -> readCore >>= (\core -> return ((core,eph), eph))

instance RepoWriter RW where
    updateHead com = RW $ \eph -> return ((),Ephemera com (toRebase eph))
    updateToRs toRs = RW $ \eph -> return ((),Ephemera (headC eph) toRs)

getHC :: RepoReader m => m (Commit Hash)
getHC = liftM (headC . snd) readRepo

getToRs :: RepoReader m => m ([Commit Hash])
getToRs = liftM (toRebase . snd) readRepo

-- All the information in the repository.
-- An append-only Core, and a changing Ephemera.
type Repo = (Core, Ephemera)

-- The changing part of the repository, allows the repository to switch states.
data Ephemera = Ephemera { headC :: Commit Hash-- Current checked-out commit
                         , toRebase :: [Commit Hash]
                            -- Mid-rebase, the commits that still need to be
                            -- handled.
                         } deriving Show

instance Serialize Ephemera where
   put (Ephemera h toR) = put h >> put toR
   get = Ephemera <$> get <*> get

-- An "empty" Repo with a single empty Commit as the head.
initRepo :: Repo
initRepo = let core@(commitSet,_) = initCore
            in (core,Ephemera (head $ Set.toList commitSet) [])

-- Lifts Repo Writer into IO
repoToIO :: RW a -> Repo -> IO (a,Repo)
repoToIO (RW f) (c,eph) = do
    ((a,eph'),c') <- coreToIO (f eph) c
    return (a,(c',eph'))

repoToGen :: RW a -> Repo -> Gen (a,Repo)
repoToGen (RW f) (c,eph) = do
    ((a,eph'),c') <- coreToGen (f eph) c
    return (a,(c',eph'))

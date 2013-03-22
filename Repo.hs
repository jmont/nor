{-# LANGUAGE Rank2Types #-}

module Repo
  ( Repo
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

import Core

liftState :: Monad m => m a -> b -> m (a,b)
liftState m s = m >>= (\a -> return (a,s))

-- The changing part of the repository, allows the repository to switch states.
data Ephemera = Ephemera { headC :: HashCommit-- Current checked-out commit
                         , toRebase :: [HashCommit]
                            -- Mid-rebase, the commits yet to be handled.
                         } deriving Show

instance Serialize Ephemera where
   put (Ephemera h toR) = put h >> put toR
   get = Ephemera <$> get <*> get

-- All the information in the repository.
-- An append-only Core, and a changing Ephemera.
type Repo = (Core, Ephemera)

-- An "empty" Repo with a single empty Commit as the head.
initRepo :: Repo
initRepo = let core@(commitSet,_) = initCore
            in (core,Ephemera (head $ Set.toList commitSet) [])

------------------------------------------------------------------------------

data RR a = RR { rr :: Repo -> a }

class CoreReader m => RepoReader m where
    readEphemera :: m Ephemera

instance Monad RR where
    return a = RR $ const a
    (RR r) >>= k = RR $ \repo -> rr (k (r repo)) repo

instance CoreReader RR where
    readCore = RR $ fst

instance RepoReader RR where
    readEphemera = RR $ snd

------------------------------------------------------------------------------

data RW a = RW { wr :: CoreExtender m => Ephemera -> m (a, Ephemera) }

class (CoreExtender m, RepoReader m) => RepoWriter m where
    updateHead :: HashCommit -> m ()
    updateToRs :: [HashCommit] -> m ()

instance Monad RW where
    return a = RW $ \eph -> return (a, eph)
    (RW m) >>= k = RW $ \eph -> do { (b, eph') <- m eph ; wr (k b) eph' }

instance CoreReader RW where
    readCore = RW $ liftState readCore

-- Additionaly updates the head commit
instance CoreExtender RW where
    addCommit dc = RW (liftState (addCommit dc)) >>=
                       (\com -> updateHead com >> return com)

instance RepoReader RW where
    readEphemera = RW $ \eph -> return (eph, eph)

instance RepoWriter RW where
    updateHead com = RW $ \eph -> return ((),Ephemera com (toRebase eph))
    updateToRs toRs = RW $ \eph -> return ((),Ephemera (headC eph) toRs)

------------------------------------------------------------------------------

getHC :: RepoReader m => m (HashCommit)
getHC = liftM headC readEphemera

getToRs :: RepoReader m => m ([HashCommit])
getToRs = liftM toRebase readEphemera

-- Lifts Repo Writer into IO
repoToIO :: RW a -> Repo -> IO (a,Repo)
repoToIO (RW f) (c,eph) = do
    ((a,eph'),c') <- coreToIO (f eph) c
    return (a,(c',eph'))

repoToGen :: RW a -> Repo -> Gen (a,Repo)
repoToGen (RW f) (c,eph) = do
    ((a,eph'),c') <- coreToGen (f eph) c
    return (a,(c',eph'))

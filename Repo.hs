{-# LANGUAGE Rank2Types #-}
module Repo
where
import Control.Applicative
import Data.Serialize
import qualified Data.Set as Set

import ObjectStore
import Core

class CoreReader m => RepoReader m where
    readRepo :: m Repo

class (CoreExtender m, RepoReader m) => RepoWriter m where
    updateHead :: Commit Hash -> m ()
    updateToR :: [Commit Hash] -> m ()

data RR a = RR { rr :: Repo -> a }
data RW a = RW { wr :: CoreExtender m => Ephemera -> m (a, Ephemera) }

instance Monad RR where
    return a = RR $ const a
    (RR r) >>= k = RR $ \world -> rr (k (r world)) world

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
    addCommit' fs pc = RW $ \eph -> addCommit' fs pc >>= (\com -> return (com,Ephemera com (toRebase eph)))
    --cxtoww (addCommit' fs pc) >>= (\com -> updateHead com >> return com)
    --  where cxtoww :: CX a -> RW a
    --        cxtoww cx = RW $ \(core,eph) -> let (a,core') = (xc cx) core in (a,(core',eph))

instance RepoReader RW where
    readRepo = RW $ \eph -> readCore >>= (\core -> return ((core,eph), eph))

instance RepoWriter RW where
    updateHead com = RW $ \eph -> return ((),Ephemera com (toRebase eph))
    updateToR toR = RW $ \eph -> return ((),Ephemera (headC eph) toR)

-- All the information in the repository. An append-only Core, and a changing
-- Ephemera.
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

getHead :: RepoReader m => m (Commit Hash)
getHead = readRepo >>= (\(_,eph) -> return $ headC eph)

-- An "empty" Repo with a single empty Commit as the head.
initRepo :: Repo
initRepo = let core@(commitSet,_) = initCore
            in (core,Ephemera (head $ Set.toList commitSet) [])

writeRepo :: RW a -> Repo -> IO (a,Repo)
writeRepo (RW f) (c,eph) = do
    ((a,eph'),c') <- coreToIO (f eph) c
    return (a,(c',eph'))

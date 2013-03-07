{-# LANGUAGE Rank2Types #-}
module World
where
import Control.Applicative
import Data.Serialize
import qualified Data.Set as Set

import ObjectStore
import Core

class CoreReader m => WorldReader m where
    readWorld :: m World

class (CoreExtender m, WorldReader m) => WorldWriter m where
    updateHead :: Commit Hash -> m ()
    updateToR :: [Commit Hash] -> m ()

data WR a = WR { rw :: World -> a }
data WW a = WW { ww :: CoreExtender m => Ephemera -> m (a, Ephemera) }

instance Monad WR where
    return a = WR $ const a
    (WR r) >>= k = WR $ \world -> rw (k (r world)) world

instance CoreReader WR where
    readCore = WR $ fst

instance WorldReader WR where
    readWorld = WR $ id

instance Monad WW where
    return a = WW $ \eph -> return (a, eph)
    (WW m) >>= k = WW $ \eph -> do { (b, eph') <- m eph ; ww (k b) eph' }

instance CoreReader WW where
    readCore = WW $ \eph -> readCore >>= (\core -> return (core,eph))

-- Additionaly updates the head commit
instance CoreExtender WW where
    addCommit' fs pc = WW $ \eph -> addCommit' fs pc >>= (\com -> return (com,Ephemera com (toRebase eph)))
    --cxtoww (addCommit' fs pc) >>= (\com -> updateHead com >> return com)
    --  where cxtoww :: CX a -> WW a
    --        cxtoww cx = WW $ \(core,eph) -> let (a,core') = (xc cx) core in (a,(core',eph))

instance WorldReader WW where
    readWorld = WW $ \eph -> readCore >>= (\core -> return ((core,eph), eph))

instance WorldWriter WW where
    updateHead com = WW $ \eph -> return ((),Ephemera com (toRebase eph))
    updateToR toR = WW $ \eph -> return ((),Ephemera (headC eph) toR)

-- All the information in the repository. An append-only Core, and a changing
-- Ephemera.
type World = (Core, Ephemera)

-- The changing part of the repository, allows the repository to switch states.
data Ephemera = Ephemera { headC :: Commit Hash-- Current checked-out commit
                         , toRebase :: [Commit Hash]
                            -- Mid-rebase, the commits that still need to be
                            -- handled.
                         } deriving Show

instance Serialize Ephemera where
   put (Ephemera h toR) = put h >> put toR
   get = Ephemera <$> get <*> get

getHead :: WorldReader m => m (Commit Hash)
getHead = readWorld >>= (\(_,eph) -> return $ headC eph)

-- An "empty" World with a single empty Commit as the head.
initWorld :: World
initWorld = let core@(commitSet,_) = initCore
            in (core,Ephemera (head $ Set.toList commitSet) [])

writeRepo :: WW a -> World -> IO (a,World)
writeRepo (WW f) (c,eph) = do
    ((a,eph'),c') <- coreToIO (f eph) c
    return (a,(c',eph'))

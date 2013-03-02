module World
where
import Control.Applicative
import Data.Serialize
import qualified Data.Set as Set

import Core

class CoreReader m => WorldReader m where
    readWorld :: m World

class (CoreExtender m, WorldReader m) => WorldWriter m where
    updateHead :: Commit -> m ()
    updateToR :: [Commit] -> m ()

data WR a = WR { rw :: World -> a }
data WW a = WW { ww :: World -> (a, World) }

instance Monad WR where
    return = unimp
    m >>= k = unimp

instance CoreReader WR where
    readCore = unimp

instance WorldReader WR where
    readWorld = unimp


instance Monad WW where
    return = unimp
    m >>= k = unimp

instance CoreReader WW where
    readCore = unimp

instance CoreExtender WW where
    addFile = unimp
    addCommit' = unimp

instance WorldReader WW where
    readWorld = unimp

instance WorldWriter WW where
    updateHead = unimp
    updateToR = unimp

-- All the information in the repository. An append-only Core, and a changing
-- Ephemera.
type World = (Core, Ephemera)

-- The changing part of the repository, allows the repository to switch states.
data Ephemera = Ephemera { headC :: Commit -- Current checked-out commit
                         , toRebase :: [Commit]
                            -- Mid-rebase, the commits that still need to be
                            -- handled.
                         } deriving Show

instance Serialize Ephemera where
   put (Ephemera h toR) = put h >> put toR
   get = Ephemera <$> get <*> get


-- An "empty" World with a single empty Commit as the head.
initWorld :: World
initWorld = let core@(commitSet,_) = initCore
            in (core,Ephemera (head $ Set.toList commitSet) [])

_createRepo :: IO () -- writes an initial core
_createRepo = unimp

_updateRepo :: WW a -> IO a
_updateRepo = unimp

_loadRepo :: WR a -> IO a
_loadRepo = unimp

unimp :: a
unimp = error "not impelmented"

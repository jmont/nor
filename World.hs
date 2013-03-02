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

--wrtocr :: WR a -> CX a
--wrtocr (WW f) = CX $ \c ->
--
--crtowr :: CX a -> WR a
--crtowr (CX f) WR $ \(core, eph) = (snd (f core), eph)

instance Monad WR where
    return a = WR $ const a
    (WR r) >>= k = WR $ \world -> rw (k (r world)) world

instance CoreReader WR where
    readCore = WR $ fst

instance WorldReader WR where
    readWorld = WR $ id


instance Monad WW where
    return a = WW $ \w -> (a, w)
    (WW m) >>= k = WW $ \w -> let (b, w') = m w in ww (k b) w'

instance CoreReader WW where
    readCore = WW $ \w -> (fst w, w)

instance CoreExtender WW where
    addFile = unimp
    addCommit' = unimp

instance WorldReader WW where
    readWorld = WW $ \w -> (w, w)

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

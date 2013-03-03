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
data WW a = WW { ww :: World -> (a, World) }

crtowr :: CR a -> WR a
crtowr cr = WR $ \(core,eph) -> (rc cr) core

cxtoww :: CX a -> WW a
cxtoww cx = WW $ \(core,eph) ->
      let (a,core') = (xc cx) core
      in (a,(core',eph))

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
    addCommit' fs hash = WW $ \(core, eph) ->
        let (com, core') = (xc (addCommit' fs hash)) core
        in (com, (core', Ephemera com (toRebase eph)))

instance WorldReader WW where
    readWorld = WW $ \w -> (w, w)

instance WorldWriter WW where
    updateHead com = WW $ \(c,eph) -> ((),(c,Ephemera com (toRebase eph)))
    updateToR toR = WW $ \(c,eph) -> ((),(c,Ephemera (headC eph) toR))

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


-- An "empty" World with a single empty Commit as the head.
initWorld :: World
initWorld = let core@(commitSet,_) = initCore
            in (core,Ephemera (head $ Set.toList commitSet) [])

writeRepo :: Show a => WW a -> World -> IO World
writeRepo (WW f) w =
    let (s, w') = f w
    in print s >> return w'

readRepo:: Show a => WR a -> World -> IO ()
readRepo (WR f) w = print (f w)

unimp :: a
unimp = error "not impelmented"

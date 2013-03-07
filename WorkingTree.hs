module WorkingTree
where

import qualified Data.List as List

import Core
import ObjectStore
import World

data FileSystem = FS { inputFs  :: [File]
                     , outputFs :: [File] }

type WorkingTree = (World,FileSystem)

data WTW a = WTW { wwt :: WorkingTree -> (a,WorkingTree) }
data WTR a = WTR { rwt :: WorkingTree -> a }

class WorldReader m => WorkingTreeReader m where
    readTree :: m WorkingTree

class (WorldWriter m, WorkingTreeReader m) => WorkingTreeWriter m where
    changeHeadTo :: Commit Hash -> m ()

instance Monad WTR where
    return a = WTR $ const a
    (WTR f) >>= k = WTR $ \wt -> rwt (k (f wt)) wt

instance Monad WTW where
    return a = WTW $ \wt -> (a,wt)
    (WTW f) >>= k = WTW $ \wt -> let (b, wtw') = f wt in wwt (k b) wtw'

instance WorkingTreeReader WTR where
    readTree = WTR id

instance WorldReader WTR where
    readWorld = WTR $ fst

instance CoreReader WTR where
    readCore = WTR $ fst . fst

instance WorkingTreeWriter WTW where
    changeHeadTo com = getHead >>= getFilesForCom >>= deleteFiles >>
                       updateHead com >> getFilesForCom com >>= restoreFiles
      where deleteFiles  fs = WTW $ \(w,FS inf ouf) -> ((),(w, FS inf (ouf List.\\ fs)))
            restoreFiles fs = WTW $ \(w,FS inf ouf) -> ((),(w, FS inf (List.nub (ouf ++ fs))))

instance WorldWriter WTW where
    updateHead com = WTW $ \((c,eph),fs) -> ((),((c, Ephemera com (toRebase eph)),fs))
    updateToR  toR = WTW $ \((c,eph),fs) -> ((),((c,Ephemera (headC eph) toR),fs))

instance WorkingTreeReader WTW where
    readTree = WTW $ \wtw -> (wtw, wtw)

instance WorldReader WTW where
   readWorld = WTW $ \wtw -> (fst wtw, wtw)

instance CoreExtender WTW where
   addCommit' fs hash = cxtowtw (addCommit' fs hash)
    where cxtowtw :: CX a -> WTW a
          cxtowtw cx = WTW $ \((core,eph),fs) -> let (a,core') = (xc cx) core in (a,((core',eph),fs))

instance CoreReader WTW where
   readCore = WTW $ \wtw -> (fst (fst wtw), wtw)

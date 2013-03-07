module WorkingTree
where

import Control.Monad
import qualified Control.Exception as E
import qualified Data.ByteString as S
import Data.Serialize
import qualified Data.Set as Set
import System.Directory
import System.IO

import Core
import ObjectStore
import World

data FileSystem = FS { trackedPaths :: Set.Set String
                     , currFiles :: [File] }

instance Serialize FileSystem where
  put (FS tps _) = put tps
  get = liftM2 FS get (return [])

type WorkingTree = (World,FileSystem)

data WTW a = WTW { wwt :: WorkingTree -> (a,WorkingTree) }
data WTR a = WTR { rwt :: WorkingTree -> a }

class WorldReader m => WorkingTreeReader m where
    readFs :: m [File]


class (WorldWriter m, WorkingTreeReader m) => WorkingTreeWriter m where
    trackFile :: String -> m ()
    changeHeadTo :: Commit Hash -> m ()

instance Monad WTR where
    return a = WTR $ const a
    (WTR f) >>= k = WTR $ \wt -> rwt (k (f wt)) wt

instance Monad WTW where
    return a = WTW $ \wt -> (a,wt)
    (WTW f) >>= k = WTW $ \wt -> let (b, wtw') = f wt in wwt (k b) wtw'

instance WorkingTreeReader WTR where
    readFs = WTR $ currFiles . snd

instance WorldReader WTR where
    readWorld = WTR $ fst

instance CoreReader WTR where
    readCore = WTR $ fst . fst

instance WorkingTreeWriter WTW where
    trackFile path = WTW $ \(w, FS tpaths cfs) -> ((),(w, FS (Set.insert path tpaths) cfs))
    changeHeadTo com = deleteFiles >> updateHead com >> getFilesForCom com >>= restoreFiles
      where deleteFiles     = WTW $ \(w,FS _ _) -> ((),(w, FS Set.empty []))
            restoreFiles fs = WTW $ \(w,FS _ _) -> ((),(w, FS (Set.fromList (map path fs)) fs))

instance WorldWriter WTW where
    updateHead com = WTW $ \((c,eph),fs) -> ((),((c, Ephemera com (toRebase eph)),fs))
    updateToR  toR = WTW $ \((c,eph),fs) -> ((),((c,Ephemera (headC eph) toR),fs))

instance WorkingTreeReader WTW where
    readFs = WTW $ \wtw -> ((currFiles . snd) wtw, wtw)

instance WorldReader WTW where
   readWorld = WTW $ \wtw -> (fst wtw, wtw)

instance CoreExtender WTW where
   addCommit' fs hash = wwtowtw (addCommit' fs hash)
    where wwtowtw :: WW a -> WTW a
          wwtowtw (WW f) = WTW $ \(w,fs) -> let (a,w') = f w in (a,(w',fs))

instance CoreReader WTW where
   readCore = WTW $ \wtw -> (fst (fst wtw), wtw)

--IO dealing stuff
getFile :: String -> IO File
getFile p = do
   contents <- readFile ("./"++p)
   return $ File p (lines contents)

getFiles :: [String] -> IO [File]
getFiles ps = mapM getFile ps

-- Remove the file in the filesystem at the File's path.
deleteFile :: File -> IO ()
deleteFile (File p _) = do
    fileExists <- doesFileExist p
    when fileExists $ removeFile p

-- Remove the file in the filesystem at path of each File.
deleteFiles :: [File] -> IO ()
deleteFiles fs = do
    mapM_ deleteFile fs
    return ()

-- Write the contents of the File to its path in the filesystem.
restoreFile :: File -> IO ()
restoreFile (File p cs) = do
    handle <- openFile p WriteMode
    hPutStr handle $ unlines cs
    hClose handle

-- Write the contents of multiple Files to their path in the filesystem.
restoreFiles :: [File] -> IO ()
restoreFiles fs = do
    mapM_ restoreFile fs
    return ()

-- Serialize the world to the filesystem.
saveWTree :: String -> WTR (IO ())
saveWTree worldPath = WTR (\wtw -> openFile worldPath WriteMode >>=
                                (\h -> S.hPutStr h (encode wtw) >> hClose h ))

-- Create the directory in which to save program data.
createProgDir :: String -> IO ()
createProgDir progDirPath = createDirectory progDirPath

-- Unserialize the World from the filesystem. If no such serialized file
-- exists, create the directory in which to save it, and use an empty World.
getWTree :: String -> String -> IO WorkingTree
getWTree progDirPath worldPath = do
    eitherW <- getWTree'
    case eitherW of
        Left _ -> createProgDir progDirPath >> return (initWorld,FS Set.empty [])
        Right wtw -> return wtw
    where getWTree' :: IO (Either String WorkingTree)
          getWTree' = E.catch
              (do handle <- openFile worldPath ReadMode
                  encodedW <- S.hGetContents handle
                  hClose handle
                  return $ decode encodedW)
              (\(e) -> hPrint stderr (e :: E.IOException) >>
                  return (Left "No World found."))

--TODO this is terrible code
runWorkingTree :: Show a => String -> String -> WTW a -> IO ()
runWorkingTree progDirPath worldPath (WTW f) = do
    (w,FS tfs _) <- getWTree progDirPath worldPath
    files <- getFiles (Set.toList tfs)
    let initWT = (w, FS tfs files)
    deleteFiles files
    let (a, wtw@(_,fileSys')) = f initWT
    restoreFiles (currFiles fileSys')
    rwt (saveWTree worldPath) wtw
    print a

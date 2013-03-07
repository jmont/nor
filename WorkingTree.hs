{-# LANGUAGE Rank2Types #-}
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

liftState :: Monad m => m a -> b -> m (a,b)
liftState m s = m >>= (\a -> return (a,s))

data FileSystem = FS { trackedPaths :: Set.Set FilePath
                     , currFiles :: [File] }

instance Serialize FileSystem where
  put (FS tps _) = put tps
  get = liftM2 FS get (return [])

type WorkingTree = (World,FileSystem)

data WTW a = WTW { wwt :: WorldWriter m => FileSystem -> m (a,FileSystem) }
data WTR a = WTR { rwt :: WorkingTree -> a }

class WorldReader m => WorkingTreeReader m where
    readFs :: m [File]


class (WorldWriter m, WorkingTreeReader m) => WorkingTreeWriter m where
    trackFile :: FilePath -> m ()
    changeHeadTo :: Commit Hash -> m ()

instance Monad WTR where
    return a = WTR $ const a
    (WTR f) >>= k = WTR $ \wt -> rwt (k (f wt)) wt

instance Monad WTW where
    return a = WTW $ \fs -> return (a,fs)
    (WTW f) >>= k = WTW $ \fs -> do { (b, fs') <- f fs ; wwt (k b) fs' }

instance WorkingTreeReader WTR where
    readFs = WTR $ currFiles . snd

instance WorldReader WTR where
    readWorld = WTR $ fst

instance CoreReader WTR where
    readCore = WTR $ fst . fst

instance WorkingTreeWriter WTW where
    trackFile path = WTW $ \(FS tpaths cfs) -> return ((),FS (Set.insert path tpaths) cfs)
    changeHeadTo com = deleteFiles >> updateHead com >> getFilesForCom com >>= restoreFiles
      where deleteFiles     = WTW $ \_ -> return ((),FS Set.empty [])
            restoreFiles fs = WTW $ \_ -> return ((),FS (Set.fromList (map path fs)) fs)

instance WorldWriter WTW where
    updateHead com = WTW $ liftState $ updateHead com
    updateToR  toR = WTW $ liftState $ updateToR toR

instance WorkingTreeReader WTW where
    readFs = WTW $ \fs -> return (currFiles fs, fs)

instance WorldReader WTW where
   readWorld = WTW $ liftState $ readWorld

instance CoreExtender WTW where
   addCommit' fs pc = WTW $ liftState $ addCommit' fs pc

instance CoreReader WTW where
   readCore = WTW $ liftState readCore

--IO dealing stuff
getFile :: String -> IO File
getFile p = do
   contents <- readFile ("./"++p)
   return $ File p (lines contents)

getFiles :: [FilePath] -> IO [File]
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
saveWTree :: FilePath -> WTR (IO ())
saveWTree worldPath = WTR (\wtw -> openFile worldPath WriteMode >>=
                                (\h -> S.hPutStr h (encode wtw) >> hClose h ))

-- Create the directory in which to save program data.
createProgDir :: FilePath -> IO ()
createProgDir progDirPath = createDirectory progDirPath

-- Unserialize the World from the filesystem. If no such serialized file
-- exists, create the directory in which to save it, and use an empty World.
loadWTree :: FilePath -> FilePath -> IO WorkingTree
loadWTree progDirPath worldPath = do
    eitherW <- loadWTree'
    case eitherW of
        Left _ -> createProgDir progDirPath >> return (initWorld,FS Set.empty [])
        Right wtw -> return wtw
    where loadWTree' :: IO (Either String WorkingTree)
          loadWTree' = E.catch
              (do handle <- openFile worldPath ReadMode
                  encodedW <- S.hGetContents handle
                  hClose handle
                  return $ decode encodedW)
              (\(e) -> hPrint stderr (e :: E.IOException) >>
                  return (Left "No World found."))

runWorkingTree :: FilePath -> FilePath -> WTW a -> IO a
runWorkingTree progDirPath worldPath (WTW f) = do
    (w,FS tfs _) <- loadWTree progDirPath worldPath
    files <- getFiles (Set.toList tfs)
    ((a,fileSys'),world) <- writeRepo (f (FS tfs files)) w
    deleteFiles files >> restoreFiles (currFiles fileSys')
    rwt (saveWTree worldPath) (world,fileSys')
    return a

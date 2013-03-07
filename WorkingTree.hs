module WorkingTree
where

import Control.Monad
import qualified Control.Exception as E
import qualified Data.ByteString as S
import qualified Data.List as List
import Data.Serialize
import System.Directory
import System.IO

import Core
import ObjectStore
import World

data FileSystem = FS { inputFs  :: [File]
                     , outputFs :: [File] }

type WorkingTree = (World,FileSystem)

data WTW a = WTW { wwt :: WorkingTree -> (a,WorkingTree) }
data WTR a = WTR { rwt :: WorkingTree -> a }

class WorldReader m => WorkingTreeReader m where
    readFs :: m [File]


class (WorldWriter m, WorkingTreeReader m) => WorkingTreeWriter m where
    changeHeadTo :: Commit Hash -> m ()

instance Monad WTR where
    return a = WTR $ const a
    (WTR f) >>= k = WTR $ \wt -> rwt (k (f wt)) wt

instance Monad WTW where
    return a = WTW $ \wt -> (a,wt)
    (WTW f) >>= k = WTW $ \wt -> let (b, wtw') = f wt in wwt (k b) wtw'

instance WorkingTreeReader WTR where
    readFs = WTR $ inputFs . snd

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
    readFs = WTW $ \wtw -> ((inputFs . snd) wtw, wtw)

instance WorldReader WTW where
   readWorld = WTW $ \wtw -> (fst wtw, wtw)

instance CoreExtender WTW where
   addCommit' fs hash = wwtowtw (addCommit' fs hash)
    where wwtowtw :: WW a -> WTW a
          wwtowtw (WW f) = WTW $ \(w,fs) -> let (a,w') = f w in (a,(w',fs))

instance CoreReader WTW where
   readCore = WTW $ \wtw -> (fst (fst wtw), wtw)

--IO dealing stuff

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
saveWorld :: String -> WorldReader m => m (IO ())
saveWorld worldPath = readWorld >>= (\w -> return (openFile worldPath WriteMode >>=
                                (\h -> S.hPutStr h (encode w) >> hClose h )))

-- Create the directory in which to save program data.
createProgDir :: String -> IO ()
createProgDir progDirPath = createDirectory progDirPath

-- Unserialize the World from the filesystem. If no such serialized file
-- exists, create the directory in which to save it, and use an empty World.
getWorld :: String -> String -> IO World
getWorld progDirPath worldPath = do
    eitherW <- getWorld'
    case eitherW of
        Left _ -> createProgDir progDirPath >> return initWorld
        Right w -> return w
    where getWorld' :: IO (Either String World)
          getWorld' = E.catch
              (do handle <- openFile worldPath ReadMode
                  encodedW <- S.hGetContents handle
                  hClose handle
                  return $ decode encodedW)
              (\(e) -> hPrint stderr (e :: E.IOException) >>
                  return (Left "No World found."))

--TODO this is terrible code
runWorkingTree :: Show a => String -> String -> WTW a -> IO ()
runWorkingTree progDirPath worldPath (WTW f) = do
    (core,eph) <- getWorld progDirPath worldPath
    let (WR f') = getFilesForCom (headC eph) >>= (\fs -> return ((core,eph),FS fs fs))
    let initWT = f' (core,eph)
    let (a, (w',fileSys')) = f initWT
    deleteFiles (inputFs fileSys')
    restoreFiles (outputFs fileSys')
    rw (saveWorld worldPath) w'
    print a

{-# LANGUAGE Rank2Types #-}
module WorkingTree
  ( WorkingTreeReader
  , WorkingTreeWriter
  , runWorkingTree
  , workingTreeToGen
  , readFs
  , trackFile
  , checkoutCom
  , applyFileTrans
  , initFS
  )
where

import Control.Monad
import qualified Control.Exception as E
import qualified Data.ByteString as S
import Data.Serialize
import qualified Data.Set as Set
import System.Directory
import System.IO
import Test.QuickCheck.Gen

import Core
import ObjectStore
import Repo

data FileSystem = FS { _trackedPaths :: Set.Set FilePath
                     , currFiles :: [File] }

instance Serialize FileSystem where
  put (FS tps _) = put tps
  get = liftM2 FS get (return [])

type WorkingTree = (Repo,FileSystem)

data WTW a = WTW { wwt :: RepoWriter m => FileSystem -> m (a,FileSystem) }
data WTR a = WTR { rwt :: WorkingTree -> a }

class RepoReader m => WorkingTreeReader m where
    readFs :: m [File]

class (RepoWriter m, WorkingTreeReader m) => WorkingTreeWriter m where
    trackFile :: FilePath -> m ()
    checkoutCom :: Commit Hash -> m ()
    -- Does this expose too much?
    applyFileTrans :: ([File] -> [File]) -> m ()

instance Monad WTR where
    return a = WTR $ const a
    (WTR f) >>= k = WTR $ \wt -> rwt (k (f wt)) wt

instance Monad WTW where
    return a = WTW $ \fs -> return (a,fs)
    (WTW f) >>= k = WTW $ \fs -> do { (b, fs') <- f fs ; wwt (k b) fs' }

instance WorkingTreeReader WTR where
    readFs = WTR $ currFiles . snd

instance RepoReader WTR where
    readEphemera = WTR $ (snd . fst)

instance CoreReader WTR where
    readCore = WTR $ fst . fst

instance WorkingTreeWriter WTW where
    trackFile path = WTW $ \(FS tpaths cfs) ->
       return ((),FS (Set.insert path tpaths) cfs)
    checkoutCom com = deleteFiles >> updateHead com >>
                      getFilesForCom com >>= restoreFiles
      where deleteFiles     = WTW $ \_ -> return ((),FS Set.empty [])
            restoreFiles fs = WTW $ \_ ->
                return ((),FS (Set.fromList (map path fs)) fs)
    applyFileTrans fsTran = WTW $ \(FS tpaths cfs) ->
       return $ ((),FS tpaths (fsTran cfs))

instance RepoWriter WTW where
    updateHead com = WTW $ liftState $ updateHead com
    updateToRs toRs = WTW $ liftState $ updateToRs toRs

instance WorkingTreeReader WTW where
    readFs = WTW $ \fs -> return (currFiles fs, fs)

instance RepoReader WTW where
   readEphemera = WTW $ liftState $ readEphemera

instance CoreExtender WTW where
   addCommit fs pc = WTW $ liftState $ addCommit fs pc

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

-- Unserialize the Repo from the filesystem. If no such serialized file
-- exists, create the directory in which to save it, and use an empty Repo.
loadWTree :: FilePath -> FilePath -> IO WorkingTree
loadWTree progDirPath worldPath = do
    eitherW <- loadWTree'
    case eitherW of
        Left _ -> createProgDir progDirPath >> return (initRepo,initFS)
        Right wtw -> return wtw
    where loadWTree' :: IO (Either String WorkingTree)
          loadWTree' = E.catch
              (do handle <- openFile worldPath ReadMode
                  encodedW <- S.hGetContents handle
                  hClose handle
                  return $ decode encodedW)
              (\(e) -> hPrint stderr (e :: E.IOException) >>
                  return (Left "No Repo found."))

initFS :: FileSystem
initFS = FS Set.empty []

runWorkingTree :: FilePath -> FilePath -> WTW a -> IO a
runWorkingTree progDirPath worldPath (WTW f) = do
    (w,FS tfs _) <- loadWTree progDirPath worldPath
    files <- getFiles (Set.toList tfs)
    ((a,fileSys'),world) <- repoToIO (f (FS tfs files)) w
    deleteFiles files >> restoreFiles (currFiles fileSys')
    rwt (saveWTree worldPath) (world,fileSys')
    return a

workingTreeToGen :: WTW a -> WorkingTree -> Gen (a,WorkingTree)
workingTreeToGen (WTW f) (w,fs) = do
    ((a,fs'),w') <- repoToGen (f fs) w
    return (a,(w',fs'))

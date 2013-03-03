import Control.Monad
import System.Environment
import System.Directory
import System.IO
import Data.Serialize
import qualified Control.Exception as E
import qualified Data.Set as Set
import qualified Data.ByteString as S
import qualified ObjectStore as O
import qualified Data.List as List
import qualified Control.Monad.State as State

import Core
import Nor
import Patch
import World
import ObjectStore

-- Location in which to save program data.
progDirPath :: String
progDirPath = "./.nor"
worldPath :: String
worldPath = progDirPath ++ "/world"

-- Serialize the world to the filesystem.
saveWorld :: World -> IO ()
saveWorld w = do
    handle <- openFile worldPath WriteMode
    S.hPutStr handle $ encode w
    hClose handle

-- Unserialize the World from the filesystem. If no such serialized file
-- exists, create the directory in which to save it, and use an empty World.
getWorld :: IO World
getWorld = do
    eitherW <- getWorld'
    case eitherW of
        Left _ -> createProgDir >> return initWorld
        Right w -> return w
    where getWorld' :: IO (Either String World)
          getWorld' = E.catch
              (do handle <- openFile worldPath ReadMode
                  encodedW <- S.hGetContents handle
                  hClose handle
                  return $ decode encodedW)
              (\(e) -> hPrint stderr (e :: E.IOException) >>
                  return (Left "No World found."))

-- Create the directory in which to save program data.
createProgDir :: IO ()
createProgDir = createDirectory progDirPath

-- Create a File with contents of the file at the specified path in the
-- filesystem. Error if the file doesn't exist.
getFile :: String -> IO File
getFile p = do
    contents <- readFile ("./"++p)
    return $ File p (lines contents)

-- Adds a new commit to the world containing the files specified.
-- If "-a" is the first argument, implicitly commit the current head's files.
-- The parent of the new commit is the current head.
-- The new commit becomes the current head.
commit :: World -> [String] -> IO World
commit w@((_, os), eph) ("-a":names) = do
    let Just files = mapM (O.getObject os) (cContents (headC eph))
    let paths = map path files ++ names
    commit w paths
commit (core, eph) names = do
    fs <- mapM getFile names
    let (newHead,newCore) = State.runState (addCommit fs (cid (headC eph))) core
    let w' = (newCore, Ephemera newHead (toRebase eph))
    print $ cid newHead
    return w'

-- Output the head commit and all other commits.
printCommits :: World -> IO ()
printCommits ((commits, _) , eph) = do
    putStrLn $ "HEAD: " ++ show (cid (headC eph))
    mapM_ print (Set.toList commits)
    return ()

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

-- Remove files in the current head commit. Restore the files from the commit
-- corresponding to the specified hash. This commit is made the head commit.
checkout :: World -> String -> IO World
checkout (core@(comSet, os), eph) hh =
    let h = O.hexToHash hh
        Just com = commitById core h
        Just dFiles = mapM (O.getObject os) (cContents (headC eph))
        Just rFiles = mapM (O.getObject os) (cContents com)
    in do deleteFiles dFiles
          restoreFiles rFiles
          putStrLn $ "Updated repo to " ++ hh
          return ((comSet, os), Ephemera com (toRebase eph))

-- Print the files from the commit corresponding to the specified hash.
files :: World -> String -> IO World
files w@((comSet, os), _) hh = do
    let h = O.hexToHash hh
    let com = commitByHash comSet h
    let Just files = O.getObjects os (cContents com)
    putStrLn $ "Files for " ++ hh
    mapM_ print files
    return w

-- On the commit corresponding to the specified hash, replay commits
-- between the least common ancestor of the head and the commit.
rebase :: World -> String -> IO World
rebase (core@(_,os), eph) "--continue" = do
   let toPaths = getPaths (headC eph)
   let fromPaths = getPaths . head . toRebase $ eph
   newFiles <- mapM getFile $ List.nub $ fromPaths ++ toPaths
   rebaseStop $ resolveWithFiles core (headC eph) newFiles (toRebase eph)
   where getPaths :: Commit Hash -> [Path]
         getPaths c =
            let Just files = O.getObjects os (cContents c)
            in map path files
rebase (core@(comSet,_), eph) hh =
   let toHash = O.hexToHash hh
       toCom = commitByHash comSet toHash
   in rebaseStop $ rebaseStart core (headC eph) toCom

rebaseStop :: RebaseRes -> IO World
rebaseStop (Succ core head) =
    let w' = (core, Ephemera head [])
    in checkout w' ((show . cid) head)
rebaseStop (Conf (core@(_,os)) head confs noConfs toRs lca) =
    let conflictPatches = map conflictAsPatch confs
        Just files = mapM (O.getObject os) (cContents lca)
        combinedPatches = sequenceParallelPatches (conflictPatches ++ noConfs)
        w = (core, Ephemera head toRs)
    in do
      _ <- checkout w (show (cid lca)) -- replace fs with lca's files
      restoreFiles $ applyPatches combinedPatches files
      putStrLn "Conflicts! Fix them and run nor rebase --continue"
      return w

-- Lookup a commit by its hash
commitByHash :: Set.Set (Commit Hash) -> O.Hash -> Commit Hash
commitByHash comSet h = head $ Set.toList $ Set.filter ((h==).cid) comSet

--Runs the given command with args to alter the world.
--Ensures that if mid-rebase, no other commands can be used.
dispatch :: World -> String -> [String] -> IO World
dispatch w@(_, Ephemera _ _) "rebase" args = dispatch' w "rebase" args
dispatch w@(_, Ephemera _ []) cmd args = dispatch' w cmd args
dispatch _ _ _ = error "Please continue rebasing before other commands"

dispatch' :: World -> String -> [String] -> IO World
-- Nor commands
dispatch' w "commit" ns = commit w ns
dispatch' w "tree" _ = printCommits w >> return w
dispatch' w "checkout" [h] = checkout w h
dispatch' _ "checkout" _ = error "checkout expects exactly one hash"
dispatch' w "files" [h] = files w h
dispatch' _ "files" _ = error "files expects exactly one hash"
dispatch' w "rebase" [arg] = rebase w arg
dispatch' _ "rebase" _ = error "Usage: nor rebase <--continue | hash>"
-- Default
dispatch' w _ _ = putStrLn "    ! Invalid Command" >> return w

main :: IO ()
main = do
    w <- getWorld
    args <- getArgs
    when (null args) (getProgName >>= (\pn ->
        error ("Usage: " ++ pn ++ " < commit | tree | checkout | files | rebase >")))
    w' <- dispatch w (head args) (tail args)
    saveWorld w'

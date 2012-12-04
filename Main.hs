import Control.Monad
import System.Environment
import System.Directory
import System.IO
import Data.List
import Data.Serialize
import Numeric
import qualified Control.Exception as E
import qualified Data.Set as Set
import qualified Data.ByteString as S
import Data.Serialize
import qualified ObjectStore as O
import Nor
import qualified Control.Monad.State as State
import Control.Applicative
import Patch

-- The changing part of the repository, allows the repository to switch states.
data Ephemera = Ephemera { headC :: Commit -- Current checked-out commit
                         , toRebase :: [Commit]
                            -- Mid-rebase, the commits that still need to be
                            -- handled.
                         } deriving Show

instance Serialize Ephemera where
   put (Ephemera h toR) = put h >> put toR
   get = Ephemera <$> get <*> get

-- All the information in the repository. An append-only Core, and a changing
-- Ephemera.
type World = (Core, Ephemera)

-- An "empty" World with a single empty Commit as the head.
initWorld :: World
initWorld = let core@(commitSet,os) = initCore
            in (core,Ephemera (head $ Set.toList commitSet) [])

-- Location in which to save program data.
progDirPath = "./.nor"
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
        Left err -> createProgDir >> (return initWorld)
        Right w -> return w
    where getWorld' :: IO (Either String World)
          getWorld' = E.catch
              (do handle <- openFile worldPath ReadMode
                  encodedW <- S.hGetContents handle
                  hClose handle
                  return $ decode encodedW)
              (\(e) -> hPutStrLn stderr (show (e :: E.IOException)) >>
                  (return $ Left "No World found."))

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
    let Just files = sequence $ map (O.getObject os) (hashes (headC eph))
    let paths = map path files ++ names
    commit w paths
commit w@(core, eph) names = do
    fs <- mapM getFile names
    let fhs = addHashableAs fs
    let newCommitWithFiles = createCommit fhs (Just (headC eph))
    let (newHead,newCore) = State.runState (addCommit newCommitWithFiles) core
    let w' = (newCore, Ephemera newHead (toRebase eph))
    putStrLn $ show (cid newHead)
    return w'

-- Output the head commit and all other commits.
printCommits :: World -> IO ()
printCommits ((commits, _) , eph) = do
    putStrLn $ "HEAD: " ++ (show (cid (headC eph)))
    mapM (putStrLn . show) (Set.toList commits)
    return ()

-- Remove the file in the filesystem at the File's path.
deleteFile :: File -> IO ()
deleteFile (File p _) = do
    fileExists <- doesFileExist p
    when fileExists $ removeFile p

-- Remove the file in the filesystem at path of each File.
deleteFiles :: [File] -> IO ()
deleteFiles fs = do
    mapM deleteFile fs
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
    mapM restoreFile fs
    return ()

-- Replace tracked files with the state of the files in the commit
-- corresponding to the specified hash. This commit is made the head commit.
checkout :: World -> [String] -> IO World
checkout w@((comSet, os), eph) [hh] =
    let h = O.hexToHash hh
        com = head $ Set.toList $ Set.filter ((h==).cid) comSet -- TODO add error
        files = map (O.getObject os) (hashes com)
        Just dFiles = sequence $ map (O.getObject os) (hashes (headC eph))
        Just rFiles = sequence $ map (O.getObject os) (hashes com)
    in do deleteFiles dFiles
          restoreFiles rFiles
          putStrLn $ "Updated repo to " ++ hh
          return ((comSet, os), Ephemera com (toRebase eph))

files :: World -> [String] -> IO (World)
files w@((comSet, os), headCom) [hh] = do
    let h = O.hexToHash hh
    let com = commitByHash comSet h
    let Just files = O.getObjects os (hashes com)
    putStrLn $ "Files for " ++ hh
    mapM (putStrLn.show) files
    return w

rebase :: World -> [String] -> IO (World)
rebase (core@(comSet,os), eph) ["--continue"] =
   -- implicit commit
   let toPaths = getPaths (headC eph)
       fromPaths = getPaths . head . toRebase $ eph
       pathSet = Set.fromList (fromPaths ++ toPaths)
   in commit (core, Ephemera (headC eph) (tail (toRebase eph)))
             (Set.toList pathSet) >>= rebaseContinue
   where getPaths :: Commit -> [Path]
         getPaths c =
            let Just files = O.getObjects os (hashes c)
            in map path files
rebase w@(core@(comSet,os), eph) [hh] =
   let upstreamHash = O.hexToHash hh
       upstreamCom = commitByHash comSet upstreamHash
       lca = getLca core (headC eph) upstreamCom
       toR = reverse $ takeWhile (/= lca) (ancestorList core (headC eph))
   in rebaseContinue (core, Ephemera upstreamCom toR)

commitByHash :: Set.Set Commit -> O.Hash -> Commit
commitByHash comSet h = head $ Set.toList $ Set.filter ((h==).cid) comSet

rebaseContinue :: World -> IO (World)
rebaseContinue w@(core@(comSet, os), eph) = case toRebase eph of
   [] -> putStrLn ("Updated repo to " ++ (show (cid  (headC eph)))) >> return w
   (c:cs) ->
      let hc = headC eph
          lca = getLca core hc c
          (noConfs, confs) = mergeCommit os hc c lca
      in if null confs
         then
            let mergedC = parallelPatchesToCommit lca noConfs (Just (cid hc))
                (head',core') = State.runState (addCommit mergedC) core
                w' = (core', Ephemera head' cs)
            in putStrLn ("Merged " ++ (show (cid hc)) ++ " and "
                                   ++ (show (cid c))) >>
               rebaseContinue w'
         else do
            let conflictPatches = map conflictAsPatch confs
            let Just files = sequence $ map (O.getObject os) (hashes lca)
            let combinedPatches = sequenceParallelPatches
                                    (conflictPatches ++ noConfs)
            let paths = map path files
            checkout w [show (cid lca)] -- replace fs with lca's files
            restoreFiles $ applyPatches combinedPatches files
            putStrLn "Conflicts! Fix them and run nor rebase --continue"
            return (core, Ephemera (headC eph) (toRebase eph))

--Runs the given command with args to alter the world.
--Ensures that if mid-rebase, no other commands can be used.
dispatch :: World -> String -> [String] -> IO (World)
dispatch w@(_, Ephemera hc toReb) "rebase" args = dispatch' w "rebase" args
dispatch w@(_, Ephemera hc []) cmd args = dispatch' w cmd args
dispatch _ _ _ = error "Please continue rebasing before other commands"

dispatch' :: World -> String -> [String] -> IO (World)
-- Nor commands
dispatch' w "commit" ns = commit w ns
dispatch' w "tree" _ = printCommits w >> return w
dispatch' w "checkout" h = checkout w h
dispatch' w "files" h = files w h
dispatch' w "rebase" args = rebase w args
-- Default
dispatch' w _ _ = putStrLn "    ! Invalid Command" >> return w

main = do
    w <- getWorld
    args <- getArgs
    when (args == []) (getProgName >>= (\pn ->
        error ("Usage: " ++ pn ++ " < commit | tree | checkout | files | rebase >")))
    w' <- dispatch w (head args) (tail args)
    saveWorld w'

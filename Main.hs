import Control.Monad
import System.Environment
import qualified Data.Set as Set
import qualified ObjectStore as O
import qualified Data.List as List

import Core
import Nor
import Patch
import WorkingTree
import World
import ObjectStore

-- Location in which to save program data.
progDirPath :: String
progDirPath = "./.nor"
worldPath :: String
worldPath = progDirPath ++ "/world"

-- Create a File with contents of the file at the specified path in the
-- filesystem. Error if the file doesn't exist.
getFile :: String -> IO File
getFile p = do
    contents <- readFile ("./"++p)
    return $ File p (lines contents)

getFiles :: [String] -> IO [File]
getFiles ps = mapM getFile ps

getHCFiles :: WorldReader m => m [File]
getHCFiles = getHead >>= getFilesForCom

-- Adds a new commit to the world containing the files specified.
-- The parent of the new commit is the current head.
-- The new commit becomes the current head.
commit :: WorldWriter m => [File] -> m (Commit Hash)
commit fs = do
    (_, eph) <- readWorld
    com <- addCommit' fs (cid (headC eph))
    return com

-- Output the head commit and all other commits.
tree :: WorldReader m => m String
tree = do
    ((commits, _) , eph) <- readWorld
    return $ ("HEAD: " ++ show (cid (headC eph)) ++ "\n") ++ concatMap show (Set.toList commits)

-- Remove files in the current head commit. Restore the files from the commit
-- corresponding to the specified hash. This commit is made the head commit.
checkout :: World -> String -> IO World
checkout ((cs, os), eph) hh =
    let h = O.hexToHash hh
        com = commitById cs h
        Just dFiles = mapM (O.getObject os) (cContents (headC eph))
        Just rFiles = mapM (O.getObject os) (cContents com)
    in do deleteFiles dFiles
          restoreFiles rFiles
          putStrLn $ "Updated repo to " ++ hh
          return ((cs, os), Ephemera com (toRebase eph))

checkout' :: WorldWriter m => String -> m (Commit Hash)
checkout' hh = readCore >>= (\(cs, _) ->
     let com = commitById cs $ O.hexToHash hh
     in updateHead com >> return com)

-- Print the files from the commit corresponding to the specified hash.
files :: CoreReader m => String -> m [String]
files hh = do
    (comSet, os) <- readCore
    let h = O.hexToHash hh
    let com = commitById comSet h
    let Just files = O.getObjects os (cContents com)
    return $ ("Files for " ++ hh) : map path files

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
       toCom = commitById comSet toHash
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

--Runs the given command with args to alter the world.
--Ensures that if mid-rebase, no other commands can be used.
dispatch :: World -> String -> [String] -> IO World
dispatch w@(_, Ephemera _ _) "rebase" args = dispatch' w "rebase" args
dispatch w@(_, Ephemera _ []) cmd args = dispatch' w cmd args
dispatch _ _ _ = error "Please continue rebasing before other commands"

deleteHCFiles :: WorldReader m => m (IO ())
deleteHCFiles = liftM deleteFiles getHCFiles

restoreHCFiles :: WorldReader m => m (IO ())
restoreHCFiles = liftM restoreFiles getHCFiles

dispatch' :: World -> String -> [String] -> IO World
-- Nor commands
dispatch' w "commit" ("-a":ns) =
    liftM2 (++) (getFiles ns) (readRepo' getHCFiles w) >>= return . commit >>=
    (\ww -> writeRepo ww w)
dispatch' w "commit" ns =
    (getFiles ns) >>= return . commit >>= (\ww -> writeRepo ww w)
dispatch' w "tree" _ = readRepo tree w >> return w
dispatch' w "checkout" [h] =
  join (readRepo' deleteHCFiles w) >>
  writeRepo (checkout' h) w >>=
  (\w' -> join (readRepo' restoreHCFiles w') >> return w')
dispatch' _ "checkout" _ = error "checkout expects exactly one hash"
dispatch' w "files" [h] = readRepo (files h) w >> return w -- TODO fixme
dispatch' _ "files" _ = error "files' expects exactly one hash"
dispatch' w "rebase" [arg] = rebase w arg
dispatch' _ "rebase" _ = error "Usage: nor rebase <--continue | hash>"
-- Default
dispatch' w _ _ = putStrLn "    ! Invalid Command" >> return w

main :: IO ()
main = do
    w <- getWorld progDirPath worldPath
    args <- getArgs
    when (null args) (getProgName >>= (\pn ->
        error ("Usage: " ++ pn ++ " < commit | tree | checkout | files | rebase >")))
    w' <- dispatch w (head args) (tail args)
    join $ readRepo' (saveWorld worldPath) w'

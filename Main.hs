import Control.Monad
import System.Environment
import qualified Data.Set as Set
import qualified ObjectStore as O

import Core
import WorkingTree
import Repo
import ObjectStore

-- Location in which to save program data.
progDirPath :: String
progDirPath = "./.nor"
worldPath :: String
worldPath = progDirPath ++ "/world"

-- Adds a new commit to the world containing the files specified.
-- The parent of the new commit is the current head.
-- The new commit becomes the current head.
commit :: RepoWriter m => [File] -> m (Commit Hash)
commit fs = do
    (_, eph) <- readRepo
    com <- addCommit fs (headC eph)
    return com

-- Output the head commit and all other commits.
tree :: RepoReader m => m String
tree = do
    ((commits, _) , eph) <- readRepo
    return $ ("HEAD: " ++ show (cid (headC eph)) ++ "\n") ++ concatMap show (Set.toList commits)

-- Print the files from the commit corresponding to the specified hash.
files :: CoreReader m => String -> m [String]
files hh = do
    let h = O.hexToHash hh
    com <- commitById' h
    files <- getFilesForCom com
    return $ ("Files for " ++ hh) : map path files

-- On the commit corresponding to the specified hash, replay commits
-- between the least common ancestor of the head and the commit.
--rebase :: World -> String -> IO World
--rebase (core@(_,os), eph) "--continue" = do
--   let toPaths = getPaths (headC eph)
--   let fromPaths = getPaths . head . toRebase $ eph
--   newFiles <- mapM getFile $ List.nub $ fromPaths ++ toPaths
--   rebaseStop $ resolveWithFiles core (headC eph) newFiles (toRebase eph)
--   where getPaths :: Commit Hash -> [Path]
--         getPaths c =
--            let Just files = O.getObjects os (cContents c)
--            in map path files
--rebase (core@(comSet,_), eph) hh =
--   let toHash = O.hexToHash hh
--       toCom = commitById comSet toHash
--   in rebaseStop $ rebaseStart core (headC eph) toCom

--rebaseStop :: RebaseRes -> IO World
--rebaseStop (Succ core head) =
--    let w' = (core, Ephemera head [])
--    in checkout w' ((show . cid) head)
--rebaseStop (Conf (core@(_,os)) head confs noConfs toRs lca) =
--    let conflictPatches = map conflictAsPatch confs
--        Just files = mapM (O.getObject os) (cContents lca)
--        combinedPatches = sequenceParallelPatches (conflictPatches ++ noConfs)
--        w = (core, Ephemera head toRs)
--    in do
--      _ <- checkout w (show (cid lca)) -- replace fs with lca's files
--      restoreFiles $ applyPatches combinedPatches files
--      putStrLn "Conflicts! Fix them and run nor rebase --continue"
--      return w

dispatch :: WorkingTreeWriter m => String -> [String] -> m String
dispatch "commit" [] = readFs >>= commit >>= return . show
dispatch "add" ns = add' ns >> (return $ "Tracked " ++ show ns)
  where add' [] = return ()
        add' (n:ns) = trackFile n >> add' ns
dispatch "tree" [] = tree
dispatch "files" [h] = files h >>= return . show
dispatch "checkout" [h] = commitById' (O.hexToHash h) >>= (\com -> changeHeadTo com >> return (show com))
dispatch _ _ = error "Invlaid command"

main :: IO ()
main = do
    args <- getArgs
    when (null args) (getProgName >>= (\pn ->
        error ("Usage: " ++ pn ++ " < commit | tree | checkout | files | rebase >")))
    print =<< runWorkingTree progDirPath worldPath (dispatch (head args) (tail args))

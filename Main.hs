import Control.Monad
import System.Environment
import qualified Data.Set as Set
import qualified ObjectStore as O

import Core
import Rebase
--import ObjectStore
import Repo
import WorkingTree

-- Location in which to save program data.
progDirPath :: String
progDirPath = "./.nor"
worldPath :: String
worldPath = progDirPath ++ "/world"

-- Adds a new commit to the world containing the files specified.
-- The parent of the new commit is the current head.
-- The new commit becomes the current head.
commit :: RepoWriter m => [File] -> m (HashCommit)
commit fs = do
    hc <- getHC
    pc <- dataCommitById (cid hc)
    com <- addCommit $ DataCommit (Just pc) (Set.fromList fs)
    return com

-- Output the head commit and all other commits.
tree :: RepoReader m => m String
tree = do
    hc <- getHC
    (commits,_) <- readCore
    return $ ("HEAD: " ++ show (cid hc) ++ "\n") ++ concatMap show (Set.toList commits)

-- Print the files from the commit corresponding to the specified hash.
files :: CoreReader m => String -> m [String]
files hh = do
    let h = O.hexToHash hh
    com <- commitById h
    files <- getFilesForCom com
    return $ ("Files for " ++ hh) : map path files

simpleRebase :: RepoWriter m => String -> m (HashCommit)
simpleRebase hexFoundation = do
    let hFoundation = O.hexToHash hexFoundation
    dFoundation <- dataCommitById hFoundation
    hRebase <- getHC
    dRebase <- dataCommitById $ cid hRebase
    let lca = getLca dRebase dFoundation
    let toRs = reverse $ (takeWhile (/= lca)) (ancestorList dRebase)
    let newCom = replay dFoundation toRs
    com <- addCommit $ newCom
    return com

--runRebase :: WorkingTreeWriter m => String -> m String
--runRebase hh = do
--    let toHash = O.hexToHash hh
--    res <- (commitById toHash >>= startRebase)
--    case res of Succ   -> liftM ("Updated to " ++) (getHC >>= (return . show))
--                Conf _ -> return "Conflicts! Fix them & run rebase --continue"
--
--rebaseContinue :: WorkingTreeWriter m => m String
--rebaseContinue = readFs >>= commit >> rebase >>= (\res ->
--    case res of Succ   -> liftM ("Updated to " ++) (getHC >>= (return . show))
--                Conf _ -> return "Conflicts! Fix them & run rebase --continue")
--
dispatch :: WorkingTreeWriter m => String -> [String] -> m String
--dispatch "rebase" ["--continue"] = rebaseContinue
dispatch cmd args = liftM null getToRs >>= \res -> if not res
                                                   then return "Finish rebase!"
                                                   else dispatch' cmd args

dispatch' :: WorkingTreeWriter m => String -> [String] -> m String
dispatch' "commit" [] = readFs >>= commit >>= return . show
dispatch' "add" ns = add' ns >> (return $ "Tracked " ++ show ns)
  where add' [] = return ()
        add' (n:ns) = trackFile n >> add' ns
dispatch' "tree" [] = tree
dispatch' "files" [h] = files h >>= return . show
--dispatch' "rebase" [h] = runRebase h
dispatch' "simpleRebase" [h] = simpleRebase h >>= return . show
dispatch' "checkout" [h] = commitById (O.hexToHash h) >>= (\com -> checkoutCom com >> return (show com))
dispatch' _ _ = error "Invlaid command"

main :: IO ()
main = do
    args <- getArgs
    when (null args) (getProgName >>= (\pn ->
        error ("Usage: " ++ pn ++ " < commit | tree | checkout | files | rebase >")))
    print =<< runWorkingTree progDirPath worldPath (dispatch (head args) (tail args))

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

showToRs :: RepoReader m => m String
showToRs = do
    toRs <- getToRs
    return ("Commits left to rebase: " ++ concatMap show toRs)

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
    let newCom = rebaseNoConf dFoundation dRebase
    com <- addCommit $ newCom
    return com

startRebase :: RepoWriter m => String -> m (HashCommit,Outcome [File])
startRebase hexFoundation = do
    hFoundation <- commitById $ O.hexToHash hexFoundation
    hRebase <- getHC
    lca <- getLcaH hRebase hFoundation
    toRs <- liftM (reverse . (takeWhile (/= lca))) (ancestorListH hRebase)
    updateToRs toRs
    updateHead hFoundation
    continueRebase

continueRebase :: RepoWriter m => m (HashCommit,Outcome [File])
continueRebase = do
    hFoundation <- getHC
    hToRs <- getToRs
    dFoundation <- dataCommitById $ cid hFoundation
    dToRs <- mapM (dataCommitById . cid) hToRs
    let (newCom,res) = rebaseConf dFoundation dToRs
    com <- addCommit newCom
    case res of Succ -> updateToRs [] >> return (com,Succ)
                Fail (files,newToRs) -> do
                                      updateToRs (drop (length dToRs - length newToRs) hToRs)
                                      return (com,Fail files)

handleRebaseOut :: WorkingTreeWriter m => m (HashCommit,Outcome [File]) -> m String
handleRebaseOut mRes = do
  (newH,res) <- mRes
  checkoutCom newH
  case res of
    Succ -> return $ show newH
    Fail newFiles -> applyFileTrans (\_ -> newFiles) >> return (show "Conflicts! Fix them and then run rebase --continue")

dispatch :: WorkingTreeWriter m => String -> [String] -> m String
dispatch "rebase" ["--continue"] = handleRebaseOut ((dispatch' "commit" []) >> continueRebase)
dispatch "showToRs" [] = showToRs
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
dispatch' "rebase" [h] = handleRebaseOut $ startRebase h
dispatch' "simpleRebase" [h] = simpleRebase h >>= (\com -> checkoutCom com >> return (show com))
dispatch' "checkout" [h] = commitById (O.hexToHash h) >>= (\com -> checkoutCom com >> return (show com))
dispatch' _ _ = error "Invlaid command"

main :: IO ()
main = do
    args <- getArgs
    when (null args) (getProgName >>= (\pn ->
        error ("Usage: " ++ pn ++ " < commit | tree | checkout | files | rebase >")))
    print =<< runWorkingTree progDirPath worldPath (dispatch (head args) (tail args))

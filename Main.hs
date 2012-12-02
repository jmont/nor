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
import qualified Nor as N
import qualified Control.Monad.State as State

type World = (N.Core, N.Commit)

initWorld :: World
initWorld = let core@(commitSet,os) = N.initCore
            in (core,head $ Set.toList commitSet)

progDirPath = "./.nor"
worldPath = progDirPath ++ "/world"

saveWorld :: World -> IO ()
saveWorld w = do
    handle <- openFile worldPath WriteMode
    S.hPutStr handle $ encode w
    hClose handle

getWorld' :: IO (Either String World)
getWorld' = E.catch
    (do handle <- openFile worldPath ReadMode
        encodedW <- S.hGetContents handle
        hClose handle
        return $ decode encodedW)
    (\(e) -> hPutStrLn stderr (show (e :: E.IOException)) >> (return $ Left ""))

createProgDir :: IO ()
createProgDir = E.catch
    (createDirectory progDirPath)
    (\(e) -> hPutStrLn stderr (show (e :: E.IOException)))

getWorld :: IO (World)
getWorld = do
    eitherW <- getWorld'
    case eitherW of
        Left err -> createProgDir >> (return initWorld)
        Right w -> return w

getFile :: String -> IO(N.File)
getFile p = do
    contents <- readFile ("./"++p)
    return $ N.File p (lines contents)

commit :: World -> [String] -> IO (World)
commit w@((_, os), hc) ("-a":names) = do
    let Just files = sequence $ map (O.getObject os) (N.hashes hc)
    let paths = map N.path files ++ names
    commit w paths
commit w@(core, hc) names = do
    fs <- mapM getFile names
    let fhs = N.addHashableAs fs
    let newCommitWithFiles = N.createCommit fhs (Just hc)
    let (newHead,newCore) = State.runState (N.addCommit newCommitWithFiles) core
    let w' = (newCore, newHead)
    putStrLn $ show (N.cid newHead)
    return w'

printCommits :: World -> IO ()
printCommits ((commits, _) , hc) = do
    putStrLn $ "HEAD: " ++ (show (N.cid hc))
    mapM (putStrLn.show) (Set.toList commits)
    return ()

--check if file exists
deleteFile (N.File p _) = do
    fileExists <- doesFileExist p
    when fileExists $ removeFile p

deleteFiles :: [N.File] -> IO ()
deleteFiles fs = do 
    mapM deleteFile fs 
    return ()

--create file if it doesnt exsit....
restoreFile (N.File p cs) = do
    handle <- openFile p WriteMode
    hPutStr handle $ unlines cs
    hClose handle

restoreFiles :: [N.File] -> IO ()
restoreFiles fs = do 
    mapM restoreFile fs
    return ()

checkout :: World -> [String] -> IO (World)
checkout w@((comSet, os), headCom) [hh] = do
    let h = O.hexToHash hh
    let com = head $ Set.toList $ Set.filter ((h==).N.cid) comSet
    let files = map (O.getObject os) (N.hashes com)
    let Just dFiles = sequence $ map (O.getObject os) (N.hashes headCom)
    let Just rFiles =  sequence $ map (O.getObject os) (N.hashes com)
    deleteFiles dFiles
    restoreFiles rFiles
    putStrLn $ "Updated repo to " ++ hh
    return ((comSet, os), com)

files :: World -> [String] -> IO (World)
files w@((comSet, os), headCom) [hh] = do
    let h = O.hexToHash hh
    let com = head $ Set.toList $ Set.filter ((h==).N.cid) comSet
    let Just files = sequence $ map (O.getObject os) (N.hashes com)
    putStrLn $ "Files for " ++ hh
    mapM (putStrLn.show) files
    return w 

dispatch :: N.World -> String -> [String] -> IO (N.World)
-- Nor commands
dispatch w "commit" ns = commit w ns
dispatch w "tree" _ = printCommits w >> return w
dispatch w "checkout" h = checkout w h
dispatch w "files" h = files w h
-- Default
dispatch w _ _ = putStrLn "    ! Invalid Command" >> return w

main = do
    w <- getWorld
    args <- getArgs
    when (args == []) (getProgName >>= (\pn ->
        error ("Usage: " ++ pn ++ " <command>")))
    w' <- dispatch w (head args) (tail args)
    saveWorld w'

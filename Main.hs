import Control.Monad
import System.Environment
import System.Directory
import System.IO
import Data.List
import Data.Serialize
import qualified Control.Exception as E
import qualified Data.Set as Set
import qualified Data.ByteString as S
import qualified ObjectStore as O
import qualified Nor as N

progDirPath = "./.nor"
worldPath = progDirPath ++ "/world"

saveWorld :: N.World -> IO ()
saveWorld w = do
    handle <- openFile worldPath WriteMode
    S.hPutStr handle $ encode w
    hClose handle

getWorld' :: IO (Either String N.World)
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

getWorld :: IO (N.World)
getWorld = do
    eitherW <- getWorld'
    case eitherW of
        Left err -> createProgDir >> (return N.init)
        Right w -> return w

getFile :: String -> IO(N.File)
getFile p = do
    contents <- readFile ("./"++p)
    return $ N.File p (lines contents)

commit :: N.World -> [String] -> IO (N.World)
commit w names = do
    fs <- mapM getFile names
    return $ N.commit w fs

printCommits :: N.World -> IO ()
printCommits ((commits, _) , _) = do
    mapM (putStrLn.show) (Set.toList commits)
    return ()

dispatch :: N.World -> String -> [String] -> IO (N.World)
-- Nor commands
dispatch w "commit" ns = commit w ns
dispatch w "tree" _ = printCommits w >> return w
-- Default
dispatch w _ _ = putStrLn "    ! Invalid Command" >> return w

main = do
    w <- getWorld
    args <- getArgs
    when (args == []) (getProgName >>= (\pn ->
        error ("Usage: " ++ pn ++ " <command>")))
    w' <- dispatch w (head args) (tail args)
    saveWorld w'

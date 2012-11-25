import System.Environment
import System.IO
import Data.List
import Data.Serialize
import qualified Data.ByteString as S
import qualified ObjectStore as O
import qualified Nor as N

worldPath = "./.nor/world"

saveWorld :: N.World -> IO ()
saveWorld w = do
    handle <- openFile worldPath WriteMode
    S.hPutStr handle $ encode w
    hClose handle

getWorld' :: IO (Either String N.World)
getWorld' = do
    handle <- openFile worldPath ReadMode
    encodedW <- S.hGetContents handle
    hClose handle
    return $ decode encodedW

getWorld :: IO (N.World)
getWorld = do
    eitherW <- getWorld'
    return $ case eitherW
               of Left err -> N.init
                  Right w -> w

getFile :: String -> IO(N.File)
getFile p = do
    handle <- openFile ("./"++p) ReadMode
    contents <- hGetContents handle
    hClose handle
    return $ N.File p (lines contents)

commit :: N.World -> [String] -> IO (N.World)
commit w names = do
    fs <- mapM getFile names
    return $ N.commit w fs

dispatch :: N.World -> String -> [String] -> IO (N.World)
-- Nor commands
dispatch w "commit" ns = commit w ns
-- Default
dispatch w _ _ = putStrLn "    ! Invalid Command" >> return w

main = do 
    w <- getWorld
    (cmd:args) <- getArgs
    w' <- dispatch w cmd args
    saveWorld w'


import System.Environment
import System.IO
import Data.List
import Nor

dispatch :: String -> [String] -> IO ()
dispatch "commit" = commit
-- Add other commands here!
-- dispatch "view" = view  

main = do
    (cmd:args) <- getArgs
    dispatch cmd args


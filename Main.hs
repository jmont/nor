import System.Environment
import System.IO
import Data.List
import qualified Nor as N

type Universe = (N.HashDict, N.World)

worldPath = "./.nor/world"

{-saveWorld :: N.World -> IO ()
    saveWorld w = do
    handle <- openFile worldPath WriteMode
    hPutStr handle $ show w
    hClose handle
-}

currentFiles :: N.World -> [ N.File]
currentFiles w@(_,hd,_,_) = N.getFiles hd

addNewFile cfs name = ("hash_"++name, N.File name ["newfile"]):cfs

dispatch :: Universe -> String -> [String] -> IO (Universe)
-- Nor commands
dispatch u@(cfs, w) "vcf" _ = do
    putStrLn $ show (currentFiles w)
    return u
dispatch (cfs, w) "commit" [] = return (cfs, N.lowCommit w cfs)
-- Editor commands
dispatch (cfs, w) "new" [name] = return (addNewFile cfs name, w)
dispatch u@(cfs, _) "cfs" [] = do
    putStrLn $ show cfs
    return u
dispatch u@(cfs, w) "app" [name,txt] = 
    case N.getRmFile cfs name
      of Just (f, cfs') -> 
            (let f' = N.File (N.path f) (N.contents f ++ [txt])
              in return (N.addHash cfs' ("hash_"++name) f', w))
         otherwise -> return u   
-- Default
dispatch u _ _ = putStrLn "    ! Invalid Command" >> return u

main = do 
    main' ([], N.init)
    return ()

main' :: Universe -> IO ()
main' u@(cfs, w) = do
    putStr "nor $ "
    hFlush stdout
    input <- getLine
    let (cmd:args) = words input
    if cmd /= "quit"
    then do 
        (cfs', w') <- dispatch u cmd args
        main' (cfs', w')
    else do
--        saveWorld w
        return ()
 

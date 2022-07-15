{-
    Victim Programming Language

    name inspired by libc malloc source code xd 
    source: https://code.woboq.org/userspace/glibc/malloc/malloc.c.html#3038

    LICENCE: WTFPL
-}


module Main (main) where

import Language.Parser ( parseCode )
import Language.Interpreter ( runInterpreter )
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs

    case args of
        []            -> showUsage
        (filename: _) -> do
            code <- readFile filename
            stmts <- parseCode code
            -- print stmts
            res <- runInterpreter stmts
            case res of
                Left err -> print err
                Right r  -> pure  r


showUsage :: IO ()
showUsage = putStrLn $ unlines
    [ "[ ERROR ]"
    , "  You need to pass a .v file to interpreter!" ]

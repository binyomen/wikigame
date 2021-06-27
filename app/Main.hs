module Main (main) where

import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitWith)

import Lib (playGame)

-- We should be given two command line args: the start URL, and the end URL.
main :: IO ()
main = do
    args <- getArgs
    (crawlerName, startUrl, endUrl) <- parse args
    playGame startUrl endUrl crawlerName

parse :: [String] -> IO (String, String, String)
parse [arg1, arg2, arg3] = return (arg1, arg2, arg3)
parse _ = do
    putStrLn "Invalid args"
    exitWith $ ExitFailure 1

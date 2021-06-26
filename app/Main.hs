module Main (main) where

import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitWith)

import Lib (playGame)

-- We should be given two command line args: the start URL, and the end URL.
main :: IO ()
main = do
    args <- getArgs
    (startUrl, endUrl) <- parse args
    playGame startUrl endUrl

parse :: [String] -> IO (String, String)
parse [arg1, arg2] = return (arg1, arg2)
parse _ = do
    putStrLn "Invalid args"
    exitWith $ ExitFailure 1

module Lib
    ( playGame
    ) where

playGame :: String -> String -> IO ()
playGame startUrl endUrl = putStrLn $ startUrl ++ " " ++ endUrl

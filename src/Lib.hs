module Lib
    ( playGame
    ) where

import Page (title, scrapePage)

playGame :: String -> String -> IO ()
playGame startUrl endUrl = do
    startPage <- scrapePage startUrl
    endPage <- scrapePage endUrl
    putStrLn $ (title startPage) ++ ", " ++ (title endPage)

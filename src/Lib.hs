module Lib
    ( playGame
    ) where

import Crawler (Crawler, makeCrawler, nextPage)
import Data.Maybe (fromMaybe)
import RandomCrawler (RandomCrawler, makeCrawler, nextPage)
import Page (title, url, sourceLinkText)

import Text.HTML.Scalpel (URL)

playGame :: URL -> URL -> IO ()
playGame startUrl endUrl =
    gameLoop crawler startUrl endUrl
    where
        crawler :: RandomCrawler
        crawler = makeCrawler startUrl

gameLoop :: Crawler a => a -> URL -> URL -> IO ()
gameLoop crawler currentUrl endUrl =
    if currentUrl == endUrl then
        putStrLn "\nFinished!!!"
    else do
        (newCrawler, nextPage) <- nextPage crawler
        putStrLn $
            (getSourceLinkText $ sourceLinkText nextPage) ++ " -> " ++
            (title nextPage) ++ " | " ++
            (url nextPage)
        gameLoop newCrawler (url nextPage) endUrl

getSourceLinkText :: Maybe String -> String
getSourceLinkText sourceLinkText = fromMaybe "<START_PAGE>" sourceLinkText

module Lib
    ( playGame
    ) where

import Crawler (Crawler, makeCrawler, nextPage)
import RandomCrawler (RandomCrawler, makeCrawler, nextPage)
import Page (title, url, sourceLinkText)

import Data.Maybe (fromMaybe)
import Text.HTML.Scalpel (URL)
import System.IO (hSetEncoding, stdout, utf8)

playGame :: URL -> URL -> IO ()
playGame startUrl endUrl = do
    hSetEncoding stdout utf8
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
        let slt = getSourceLinkText (sourceLinkText nextPage) ++ " -> "
        let tit = title nextPage ++ " | "
        let u = url nextPage
        putStrLn $ addSpaces slt ++ addSpaces tit ++ u
        gameLoop newCrawler (url nextPage) endUrl

getSourceLinkText :: Maybe String -> String
getSourceLinkText = fromMaybe "<START_PAGE>"

colLen :: Int
colLen = 60

addSpaces :: String -> String
addSpaces s =
    if len < colLen then
        s ++ replicate (colLen - len) ' '
    else
        s
    where
        len = length s

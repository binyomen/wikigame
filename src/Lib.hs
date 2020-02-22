module Lib
    ( playGame
    ) where

import Crawler (Crawler, makeCrawler, nextPage)
import NGramCrawler (NGramCrawler())
import Page (Page(..))

import Data.Maybe (fromMaybe)
import Text.HTML.Scalpel (URL)
import System.IO (hSetEncoding, stdout, utf8)

playGame :: URL -> URL -> IO ()
playGame startUrl endUrl = do
    hSetEncoding stdout utf8
    crawler <- makeCrawler startUrl endUrl :: IO NGramCrawler
    gameLoop crawler startUrl endUrl

gameLoop :: Crawler a => a -> URL -> URL -> IO ()
gameLoop crawler currentUrl endUrl =
    if currentUrl == endUrl then
        putStrLn "\nFinished!!!"
    else do
        (newCrawler, newPage) <- nextPage crawler
        let sourceLinkText = getSourceLinkText (p_sourceLinkText newPage) ++ " -> "
        let title = p_title newPage ++ " | "
        let url = p_url newPage
        putStrLn $ addSpaces sourceLinkText ++ addSpaces title ++ url
        gameLoop newCrawler (p_url newPage) endUrl

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

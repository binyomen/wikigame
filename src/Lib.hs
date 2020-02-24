module Lib
    ( playGame
    ) where

import Crawler (Crawler, makeCrawler, nextPage)
import NGramCrawler (NGramCrawler())
import Page (Page(..))

import Data.Maybe (fromMaybe)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Text.HTML.Scalpel (URL)
import System.IO (hSetEncoding, stdout, utf8)
import Text.Printf (printf)

playGame :: URL -> URL -> IO ()
playGame startUrl endUrl = do
    hSetEncoding stdout utf8
    crawler <- makeCrawler startUrl endUrl :: IO NGramCrawler
    startTime <- getCurrentTime
    hops <- gameLoop crawler startUrl endUrl 0
    endTime <- getCurrentTime

    putStrLn "\n"
    putStrLn $ "Finished in " ++ show hops ++ " hops"

    let diff = diffUTCTime endTime startTime
    let totalSeconds = realToFrac $ toRational diff :: Double
    let minutes = truncate $ totalSeconds / (60 :: Double) :: Int
    let seconds = totalSeconds - (fromIntegral minutes * 60)

    printf  "            %dm%0.3fs (%0.3fs)\n" minutes seconds totalSeconds

gameLoop :: Crawler a => a -> URL -> URL -> Word -> IO Word
gameLoop crawler currentUrl endUrl pagesVisited =
    if currentUrl == endUrl then
        return $ pagesVisited - 1
    else do
        (newCrawler, newPage) <- nextPage crawler
        let sourceLinkText = getSourceLinkText (p_sourceLinkText newPage) ++ " -> "
        let title = p_title newPage ++ " | "
        let url = p_url newPage
        putStrLn $ addSpaces sourceLinkText ++ addSpaces title ++ url
        gameLoop newCrawler (p_url newPage) endUrl $ pagesVisited + 1

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

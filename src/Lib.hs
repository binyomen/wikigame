{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( playGame
    ) where

import BacktrackingCrawler (BacktrackingCrawler())
import Crawler (Crawler, makeCrawler, nextPage)
import NGramCrawler (NGramCrawler())
import Page (Link(..), Page(..))
import RandomCrawler (RandomCrawler())

import Data.Maybe (fromMaybe)
import Data.Text (Text); import qualified Data.Text as T
import Data.Time.Clock (diffUTCTime, getCurrentTime, UTCTime)
import System.IO (hSetEncoding, stdout, utf8)
import Text.HTML.Scalpel (URL)
import Text.Printf (printf)

playGame :: URL -> URL -> String -> IO ()
playGame startUrl endUrl crawlerName = do
    hSetEncoding stdout utf8
    putStrLn $ "Playing the Wikipedia game from " ++ startUrl ++ " to " ++ endUrl ++ "\n"

    (startTime, hops) <- runLoopBasedOnCrawlerName startUrl endUrl crawlerName
    endTime <- getCurrentTime

    putStrLn "\n"
    putStrLn $ "Finished in " ++ show hops ++ " hops"

    let diff = diffUTCTime endTime startTime
    let totalSeconds = realToFrac $ toRational diff :: Double
    let minutes = truncate $ totalSeconds / (60 :: Double) :: Int
    let seconds = totalSeconds - (fromIntegral minutes * 60)

    printf  "            %dm%0.3fs (%0.3fs)\n" minutes seconds totalSeconds

runLoopBasedOnCrawlerName :: URL -> URL -> String -> IO (UTCTime, Word)
runLoopBasedOnCrawlerName startUrl endUrl name =
    case name of
        "random" -> runLoopOnRandomCrawler startUrl endUrl
        "ngram" -> runLoopOnNGramCrawler startUrl endUrl
        "backtracking" -> runLoopOnBacktrackingCrawler startUrl endUrl
        _ -> error "Invalid crawler name."

runLoopOnRandomCrawler :: URL -> URL -> IO (UTCTime, Word)
runLoopOnRandomCrawler startUrl endUrl = do
    crawler <- makeCrawler startUrl endUrl :: IO RandomCrawler
    runLoop crawler startUrl endUrl

runLoopOnNGramCrawler :: URL -> URL -> IO (UTCTime, Word)
runLoopOnNGramCrawler startUrl endUrl = do
    crawler <- makeCrawler startUrl endUrl :: IO NGramCrawler
    runLoop crawler startUrl endUrl

runLoopOnBacktrackingCrawler :: URL -> URL -> IO (UTCTime, Word)
runLoopOnBacktrackingCrawler startUrl endUrl = do
    crawler <- makeCrawler startUrl endUrl :: IO BacktrackingCrawler
    runLoop crawler startUrl endUrl

runLoop :: Crawler a => a -> URL -> URL -> IO (UTCTime, Word)
runLoop crawler startUrl endUrl = do
    startTime <- getCurrentTime
    hops <- gameLoop crawler startUrl endUrl 0
    return (startTime, hops)

gameLoop :: Crawler a => a -> URL -> URL -> Word -> IO Word
gameLoop crawler currentUrl endUrl pagesVisited =
    if currentUrl == endUrl then
        return $ pagesVisited - 1
    else do
        (newCrawler, newPage) <- nextPage crawler
        let sourceLinkText = getSourceLinkText (l_text . p_link $ newPage) `T.append` " -> "
        let title = p_title newPage `T.append` " | "
        let url = l_url . p_link $ newPage
        putStrLn $ T.unpack $ addSpaces sourceLinkText `T.append` addSpaces title `T.append` T.pack url
        gameLoop newCrawler url endUrl $ pagesVisited + 1

getSourceLinkText :: Maybe Text -> Text
getSourceLinkText = fromMaybe "<START_PAGE>"

columnLength :: Int
columnLength = 60

addSpaces :: Text -> Text
addSpaces t =
    if len < columnLength then
        t `T.append` T.replicate (columnLength - len) (T.singleton ' ')
    else
        t
    where
        len = T.length t

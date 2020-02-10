module RandomCrawler
    ( RandomCrawler
    , makeCrawler
    , nextPage
    ) where

import Crawler (Crawler, makeCrawler, nextPage)
import Page (Page(..), fullUrl, scrapeTitle, scrapeLinks, convertMaybe)

import System.Random (randomRIO)
import Text.HTML.Scalpel (URL, scrapeURL)

data PageData = PageData
    { page :: Page
    , links :: [(String, URL)]
    }

data RandomCrawler = RandomCrawler
    { startUrl :: URL
    , pageData :: Maybe PageData
    }

instance Crawler RandomCrawler where
    makeCrawler startUrl = RandomCrawler { startUrl = startUrl, pageData = Nothing }

    nextPage crawler =
        case pageData crawler of
            Just pageData -> do
                nextPageData <- getNextPageData pageData
                let newCrawler = crawler { pageData = Just nextPageData }
                let newPage = page nextPageData
                return (newCrawler, newPage)
            Nothing -> do
                pageData <- getPageData Nothing (startUrl crawler)
                let newCrawler = crawler { pageData = Just pageData }
                let newPage = page pageData
                return (newCrawler, newPage)

getNextPageData :: PageData -> IO PageData
getNextPageData pageData = do
    (slt, u) <- getNextPage pageData
    getPageData (Just slt) u

getPageData :: Maybe String -> URL -> IO PageData
getPageData sourceLinkText url =
    scrapeURL (fullUrl url) scraper >>= convertMaybe url
    where
        scraper = do
            title <- scrapeTitle
            links <- scrapeLinks
            let page = Page { title = title, url = url, sourceLinkText = sourceLinkText }
            return $ PageData { page = page, links = links }

getNextPage :: PageData -> IO (String, URL)
getNextPage pageData = do
    let linkList = links pageData
    let len = length linkList
    randomNumber <- randomRIO (0, len - 1)
    return $ linkList!!randomNumber

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
    { pd_page :: Page
    , pd_links :: [(String, URL)]
    }

data RandomCrawler = RandomCrawler
    { rc_startUrl :: URL
    , rc_pageData :: Maybe PageData
    }

instance Crawler RandomCrawler where
    makeCrawler startUrl = RandomCrawler { rc_startUrl = startUrl, rc_pageData = Nothing }

    nextPage crawler =
        case rc_pageData crawler of
            Just pageData -> do
                nextPageData <- getNextPageData pageData
                let newCrawler = crawler { rc_pageData = Just nextPageData }
                let newPage = pd_page nextPageData
                return (newCrawler, newPage)
            Nothing -> do
                pageData <- getPageData Nothing (rc_startUrl crawler)
                let newCrawler = crawler { rc_pageData = Just pageData }
                let newPage = pd_page pageData
                return (newCrawler, newPage)

getNextPageData :: PageData -> IO PageData
getNextPageData pageData = do
    (sourceLinkText, url) <- getNextPage pageData
    getPageData (Just sourceLinkText) url

getPageData :: Maybe String -> URL -> IO PageData
getPageData sourceLinkText url =
    scrapeURL (fullUrl url) scraper >>= convertMaybe url
    where
        scraper = do
            title <- scrapeTitle
            links <- scrapeLinks
            let page = Page { p_title = title, p_url = url, p_sourceLinkText = sourceLinkText }
            return $ PageData { pd_page = page, pd_links = links }

getNextPage :: PageData -> IO (String, URL)
getNextPage pageData = do
    let links = pd_links pageData
    let len = length links
    randomNumber <- randomRIO (0, len - 1)
    return $ links!!randomNumber

module RandomCrawler
    ( makeCrawler
    , nextPage
    , RandomCrawler
    ) where

import Crawler (Crawler, makeCrawler, nextPage)
import Page (convertMaybe, fullUrl, Link(..), Page(..), scrapeLinks, scrapeTitle)

import System.Random (randomRIO)
import Text.HTML.Scalpel (scrapeURL, URL)

data PageData = PageData
    { pd_page :: Page
    , pd_links :: [Link]
    }

data RandomCrawler = RandomCrawler
    { rc_startUrl :: URL
    , rc_pageData :: Maybe PageData
    }

instance Crawler RandomCrawler where
    makeCrawler startUrl _ = return $ RandomCrawler{rc_startUrl = startUrl, rc_pageData = Nothing}

    nextPage crawler =
        case rc_pageData crawler of
            Just pageData -> do
                nextPageData <- getNextPageData pageData
                let newCrawler = crawler{rc_pageData = Just nextPageData}
                let newPage = pd_page nextPageData
                return (newCrawler, newPage)
            Nothing -> do
                pageData <- getPageData Link{l_text = Nothing, l_url = rc_startUrl crawler}
                let newCrawler = crawler{rc_pageData = Just pageData}
                let newPage = pd_page pageData
                return (newCrawler, newPage)

getNextPageData :: PageData -> IO PageData
getNextPageData pageData = do
    link <- getNextPage pageData
    getPageData link

getPageData :: Link -> IO PageData
getPageData link =
    scrapeURL (fullUrl url) scraper >>= convertMaybe url
    where
        url = l_url link
        scraper = do
            title <- scrapeTitle
            links <- scrapeLinks
            let page = Page{p_title = title, p_link = link}
            return $ PageData{pd_page = page, pd_links = links}

getNextPage :: PageData -> IO Link
getNextPage pageData = do
    let links = pd_links pageData
    let len = length links
    randomNumber <- randomRIO (0, len - 1)
    return $ links!!randomNumber

module NGramCrawler
    ( NGramCrawler
    , makeCrawler
    , nextPage
    ) where

import Crawler (Crawler, makeCrawler, nextPage)
import NGramModel (NGramModel, makeModel, scoreText)
import Page (Page(..), fullUrl, scrapeTitle, scrapeLinks, scrapeContentText, convertMaybe)

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Data.Foldable (maximumBy)
import Data.List (sortBy)
import Text.HTML.Scalpel (URL, scrapeURL)

n :: Word
n = 1

maxTopLinks :: Word
maxTopLinks = 5

maxIndexedModels :: Word
maxIndexedModels = 10

data PageData = PageData
    { pd_page :: Page
    , pd_linkScores :: [((String, URL), Double)]
    }

data NGramCrawler = NGramCrawler
    { ngc_startUrl :: URL
    , ngc_pageData :: Maybe PageData
    , ngc_endUrlModel :: NGramModel
    , ngc_indexedModels :: [NGramModel]
    }

instance Crawler NGramCrawler where
    makeCrawler startUrl endUrl = do
        endUrlText <- getUrlContentText endUrl
        return $ NGramCrawler
            { ngc_startUrl = startUrl
            , ngc_pageData = Nothing
            , ngc_endUrlModel = makeModel n endUrlText
            , ngc_indexedModels = map (`makeModel` endUrlText) [1..10]
            }

    nextPage crawler =
        case ngc_pageData crawler of
            Just pageData -> do
                nextPageData <- getNextPageData pageData crawler
                let newCrawler = crawler { ngc_pageData = Just nextPageData }
                let newPage = pd_page nextPageData
                return (newCrawler, newPage)
            Nothing -> do
                pageData <- getPageData Nothing (ngc_startUrl crawler) crawler
                let newCrawler = crawler { ngc_pageData = Just pageData }
                let newPage = pd_page pageData
                return (newCrawler, newPage)

getNextPageData :: PageData -> NGramCrawler -> IO PageData
getNextPageData pageData crawler = do
    let (sourceLinkText, url) = getNextPage pageData
    getPageData (Just sourceLinkText) url crawler

getPageData :: Maybe String -> URL -> NGramCrawler -> IO PageData
getPageData sourceLinkText url crawler = do
    let scraper = do
            title <- scrapeTitle
            links <- scrapeLinks
            return (title, links)
    (title, links) <- scrapeURL (fullUrl url) scraper >>= convertMaybe url

    let linkNameScores = map (scoreLinkName crawler) links
    let sortedLinkNameScores = sortBy compareLinkScores linkNameScores
    let sortedLinks = map fst sortedLinkNameScores
    let topLinks = take (fromIntegral maxTopLinks) sortedLinks

    let ioList = map (scoreLink crawler) topLinks
    let mVarIoList = map ioToMVar ioList
    mVars <- listIoToIoList mVarIoList
    linkScores <- listIoToIoList $ map takeMVar mVars

    let page = Page { p_title = title, p_url = url, p_sourceLinkText = sourceLinkText }
    return $ PageData { pd_page = page, pd_linkScores = linkScores }

getNextPage :: PageData -> (String, URL)
getNextPage pageData =
    link
    where
        scores = pd_linkScores pageData
        maxScoreLink = maximumBy compareLinkScores scores
        (link, _) = maxScoreLink

getUrlContentText :: URL -> IO String
getUrlContentText url =
    scrapeURL (fullUrl url) scrapeContentText >>= convertMaybe url

scoreLinkName :: NGramCrawler -> (String, URL) -> ((String, URL), Double)
scoreLinkName crawler (sourceLinkText, url) =
    if numWords <= maxIndexedModels then
        ((sourceLinkText, url), scoreText model sourceLinkText)
    else
        ((sourceLinkText, url), 0)
    where
        numWords = fromIntegral $ length $ words sourceLinkText
        model = ngc_indexedModels crawler !! fromIntegral (numWords - 1)

scoreLink :: NGramCrawler -> (String, URL) -> IO ((String, URL), Double)
scoreLink crawler (sourceLinkText, url) =
    scrapeURL (fullUrl url) scraper >>= convertMaybe url
    where
        model = ngc_endUrlModel crawler
        scraper = do
            contentText <- scrapeContentText
            return ((sourceLinkText, url), scoreText model contentText)

compareLinkScores :: ((String, URL), Double) -> ((String, URL), Double) -> Ordering
compareLinkScores (_, score1) (_, score2) = compare score1 score2

ioToMVar :: IO a -> IO (MVar a)
ioToMVar io = do
    mVar <- newEmptyMVar
    let thread = do
            result <- io
            putMVar mVar result
    _ <- forkIO thread
    return mVar

listIoToIoList :: [IO a] -> IO [a]
listIoToIoList (first : rest) = do
    firstUnwrapped <- first
    restUnwrapped <- listIoToIoList rest
    return $ firstUnwrapped : restUnwrapped
listIoToIoList [] = return []

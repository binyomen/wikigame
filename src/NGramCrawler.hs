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
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M (empty, insert, lookup)
import Data.Set (Set)
import qualified Data.Set as S (empty, insert, notMember)
import Text.HTML.Scalpel (URL, scrapeURL)

n :: Word
n = 1

maxTopLinks :: Word
maxTopLinks = 5

maxIndexedModels :: Word
maxIndexedModels = 10

infinity :: Double
infinity = 1 / 0

data PageData = PageData
    { pd_page :: Page
    , pd_links :: [(String, URL)]
    }

data NGramCrawler = NGramCrawler
    { ngc_startUrl :: URL
    , ngc_endUrl :: URL
    , ngc_pageData :: Maybe PageData
    , ngc_endUrlModel :: NGramModel
    , ngc_indexedModels :: [NGramModel]
    , ngc_urlScores :: Map URL Double
    , ngc_visitedUrls :: Set URL
    }

instance Crawler NGramCrawler where
    makeCrawler startUrl endUrl = do
        endUrlText <- getUrlContentText endUrl
        return $ NGramCrawler
            { ngc_startUrl = startUrl
            , ngc_endUrl = endUrl
            , ngc_pageData = Nothing
            , ngc_endUrlModel = makeModel n True endUrlText
            , ngc_indexedModels = map (flip (`makeModel` False) endUrlText) [1..maxIndexedModels]
            , ngc_urlScores = M.empty
            , ngc_visitedUrls = S.empty
            }

    nextPage crawler =
        case ngc_pageData crawler of
            Just pageData -> do
                (nextPageData, newCrawlerWithoutPageData) <- getNextPageData pageData crawler
                let newCrawler = newCrawlerWithoutPageData { ngc_pageData = Just nextPageData }
                let newPage = pd_page nextPageData
                return (newCrawler, newPage)
            Nothing -> do
                pageData <- getPageData Nothing (ngc_startUrl crawler)
                let newCrawler = crawler { ngc_pageData = Just pageData }
                let newPage = pd_page pageData
                return (newCrawler, newPage)

getNextPageData :: PageData -> NGramCrawler -> IO (PageData, NGramCrawler)
getNextPageData pageData crawler = do
    (sourceLinkText, url, newCrawler) <- getNextPage crawler pageData
    nextPageData <- getPageData (Just sourceLinkText) url
    return (nextPageData, newCrawler)

getPageData :: Maybe String -> URL -> IO PageData
getPageData sourceLinkText url = do
    let scraper = do
            title <- scrapeTitle
            links <- scrapeLinks
            return (title, links)
    (title, links) <- scrapeURL (fullUrl url) scraper >>= convertMaybe url

    let page = Page { p_title = title, p_url = url, p_sourceLinkText = sourceLinkText }
    return PageData { pd_page = page, pd_links = links }

getNextPage :: NGramCrawler -> PageData -> IO (String, URL, NGramCrawler)
getNextPage crawler pageData = do
    let links = pd_links pageData

    let linkNameScores = map (scoreLinkName crawler) links
    let sortedLinkNameScores = sortBy (flip compareLinkScores) linkNameScores
    let sortedLinks = map fst sortedLinkNameScores
    let filteredLinks = filter ((`S.notMember` ngc_visitedUrls crawler) . snd) sortedLinks
    let topLinks = take (fromIntegral maxTopLinks) filteredLinks

    let ioList = map (scoreLink crawler) topLinks
    let mVarIoList = map ioToMVar ioList
    mVars <- listIoToIoList mVarIoList
    linkScores <- listIoToIoList $ map takeMVar mVars

    let maxScoreLink = maximumBy compareLinkScores linkScores
    let (sourceLinkText, url) = fst maxScoreLink

    let newUrlScores = foldr
            (\((_, u), score) m -> M.insert u score m)
            (ngc_urlScores crawler)
            linkScores
    let newVisitedUrls = S.insert url (ngc_visitedUrls crawler)

    return
        ( sourceLinkText
        , url
        , crawler { ngc_urlScores = newUrlScores, ngc_visitedUrls = newVisitedUrls }
        )

getUrlContentText :: URL -> IO String
getUrlContentText url =
    scrapeURL (fullUrl url) scrapeContentText >>= convertMaybe url

scoreLinkName :: NGramCrawler -> (String, URL) -> ((String, URL), Double)
scoreLinkName crawler (sourceLinkText, url)
    | url == ngc_endUrl crawler = ((sourceLinkText, url), infinity)
    | numWords <= maxIndexedModels = ((sourceLinkText, url), scoreText model sourceLinkText)
    | otherwise = ((sourceLinkText, url), 0)
    where
        numWords = fromIntegral $ length $ words sourceLinkText
        model = ngc_indexedModels crawler !! fromIntegral (numWords - 1)

scoreLink :: NGramCrawler -> (String, URL) -> IO ((String, URL), Double)
scoreLink crawler (sourceLinkText, url) =
    case M.lookup url $ ngc_urlScores crawler of
        Just score -> return ((sourceLinkText, url), score)
        Nothing ->
            scrapeURL (fullUrl url) scraper >>= convertMaybe url
            where
                model = ngc_endUrlModel crawler
                scraper = do
                    links <- scrapeLinks
                    if ngc_endUrl crawler `elem` map snd links then
                        return ((sourceLinkText, url), infinity)
                    else do
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

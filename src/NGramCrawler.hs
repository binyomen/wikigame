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

-- The number of words for the n-gram model.
n :: Word
n = 1

-- The maximum number of links to actually score the contents of for a page.
maxTopLinks :: Word
maxTopLinks = 5

-- The maximum number of n-gram models to use for link names. This is basically
-- the maximum number of words for a link that we'll consider.
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
    -- `Nothing` if we haven't looked at any pages yet.
    , ngc_pageData :: Maybe PageData
    -- The smoothed n-gram model we build from the destination page.
    , ngc_endUrlModel :: NGramModel
    -- A list of `maxIndexedModels` of n-gram models to be used on link names.
    -- The first is a 1-gram model, the second is a 2-gram model, etc.
    , ngc_indexedModels :: [NGramModel]
    -- A cache of scores for encountered URLs.
    , ngc_urlScoreCache :: Map URL Double
    -- A set of which URLs we have already visited.
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
            -- Create `maxIndexedModels` of n-gram models to be used on link names.
            , ngc_indexedModels = map (flip (`makeModel` False) endUrlText) [1..maxIndexedModels]
            , ngc_urlScoreCache = M.empty
            , ngc_visitedUrls = S.empty
            }

    nextPage crawler =
        case ngc_pageData crawler of
            Just pageData -> do
                -- If we have a current page, try to get the next page from it.
                (nextPageData, newCrawlerWithoutPageData) <- getNextPageData pageData crawler
                let newCrawler = newCrawlerWithoutPageData { ngc_pageData = Just nextPageData }
                let newPage = pd_page nextPageData
                return (newCrawler, newPage)
            Nothing -> do
                -- If we don't have a current page (i.e. this is the first call
                -- to `nextPage`), just parse the start URL and return.
                pageData <- getPageData Nothing (ngc_startUrl crawler)
                let newCrawler = crawler { ngc_pageData = Just pageData }
                let newPage = pd_page pageData
                return (newCrawler, newPage)

-- Get the next page, given a current page and a crawler. Return the next
-- page's data, as well as a new crawler with relevant information filled in.
getNextPageData :: PageData -> NGramCrawler -> IO (PageData, NGramCrawler)
getNextPageData pageData crawler = do
    (sourceLinkText, url, newCrawler) <- getNextPage crawler pageData
    nextPageData <- getPageData (Just sourceLinkText) url
    return (nextPageData, newCrawler)

-- Get the data for a page given the text of the link used to get to the page
-- and the page's URL.
getPageData :: Maybe String -> URL -> IO PageData
getPageData sourceLinkText url = do
    let scraper = do
            title <- scrapeTitle
            links <- scrapeLinks
            return (title, links)
    (title, links) <- scrapeURL (fullUrl url) scraper >>= convertMaybe url

    let page = Page { p_title = title, p_url = url, p_sourceLinkText = sourceLinkText }
    return PageData { pd_page = page, pd_links = links }

-- Calculate the next page given a crawler and a current page data. Return the
-- link text that reaches the next page, the URL of the next page, as well as
-- an updated crawler.
getNextPage :: NGramCrawler -> PageData -> IO (String, URL, NGramCrawler)
getNextPage crawler pageData = do
    let links = pd_links pageData

    let linkNameScores = map
            (\sourceLinkTextUrlPair -> (sourceLinkTextUrlPair, scoreLinkText crawler sourceLinkTextUrlPair))
            links
    let sortedLinkNameScores = sortBy (flip compareLinkScores) linkNameScores
    let sortedLinks = map fst sortedLinkNameScores
    -- Don't consider URLs we've already visited.
    let filteredLinks = filter ((`S.notMember` ngc_visitedUrls crawler) . snd) sortedLinks
    let topLinks = take (fromIntegral maxTopLinks) filteredLinks

    -- Score each actual link's page content on a different thread.
    let ioList = map
            (\(slt, u) -> do
                score <- scoreLink crawler u
                return ((slt, u), score))
            topLinks
    let mVarIoList = map ioToMVar ioList
    mVars <- listIoToIoList mVarIoList
    linkScores <- listIoToIoList $ map takeMVar mVars

    let maxScoreLink = maximumBy compareLinkScores linkScores
    let (sourceLinkText, url) = fst maxScoreLink

    -- Add the scored URLs to the scored map.
    let newUrlScores = foldr
            (\((_, u), score) m -> M.insert u score m)
            (ngc_urlScoreCache crawler)
            linkScores
    -- Add the new URL to the visited set.
    let newVisitedUrls = S.insert url (ngc_visitedUrls crawler)

    return
        ( sourceLinkText
        , url
        , crawler { ngc_urlScoreCache = newUrlScores, ngc_visitedUrls = newVisitedUrls }
        )

-- Get the content of the page identified by a given URL.
getUrlContentText :: URL -> IO String
getUrlContentText url =
    scrapeURL (fullUrl url) scrapeContentText >>= convertMaybe url

-- Score the text of the link given by a source link text / URL pair.
scoreLinkText :: NGramCrawler -> (String, URL) -> Double
scoreLinkText crawler (sourceLinkText, url)
    -- If the URL is the end URL, we definitely want to choose it, so score it
    -- infinity.
    | url == ngc_endUrl crawler = infinity
    -- If we are able to score the link text using one of our indexed models,
    -- do that.
    | numWordsInLinkText <= maxIndexedModels = scoreText indexedModel sourceLinkText
    -- If the link text has too many words to be scored with our indexed
    -- models, return a 0 score.
    | otherwise = 0
    where
        numWordsInLinkText = fromIntegral $ length $ words sourceLinkText
        indexedModel = ngc_indexedModels crawler !! fromIntegral (numWordsInLinkText - 1)

-- Score the contents of the link given by its URL.
scoreLink :: NGramCrawler -> URL -> IO Double
scoreLink crawler url =
    case M.lookup url $ ngc_urlScoreCache crawler of
        -- If we've already cached this URL, return the cached score.
        Just score -> return score
        Nothing ->
            scrapeURL (fullUrl url) scraper >>= convertMaybe url
            where
                model = ngc_endUrlModel crawler
                scraper = do
                    links <- scrapeLinks
                    if ngc_endUrl crawler `elem` map snd links then
                        -- If the target page contains a link to the end URL,
                        -- it should have an infinity score.
                        return infinity
                    else do
                        -- Otherwise just score the page's content.
                        contentText <- scrapeContentText
                        return $ scoreText model contentText

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

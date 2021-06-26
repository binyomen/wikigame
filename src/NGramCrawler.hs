module NGramCrawler
    ( makeCrawler
    , nextPage
    , NGramCrawler
    ) where

import Crawler (Crawler, makeCrawler, nextPage)
import MemSize (MemSize, memSize)
import NGramModel (makeModel, NGramModel, scoreText)
import Page (convertMaybe, fullUrl, Link(..), Page(..), scrapeContentText, scrapeLinks, scrapeTitle)

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Data.HashMap.Strict (HashMap); import qualified Data.HashMap.Strict as M
import Data.HashSet (HashSet); import qualified Data.HashSet as S
import Data.List (sortBy)
import Data.Maybe (fromJust)
import Data.Text (Text); import qualified Data.Text as T
import Text.HTML.Scalpel (Scraper, scrapeURL, URL)

-- The number of words for the n-gram model.
n :: Word
n = 1

-- The maximum number of links to actually score the contents of for a page.
maxTopLinks :: Word
maxTopLinks = 5

-- The maximum number of n-gram models to use for link text. This is basically
-- the maximum number of words for a link that we'll consider.
maxIndexedModels :: Word
maxIndexedModels = 10

-- The number of pages to look ahead by when determining the best next link.
lookahead :: Word
lookahead = 2

infinity :: Double
infinity = 1 / 0

data PageData = PageData
    { pd_page :: Page
    , pd_links :: [Link]
    , pd_score :: Double
    }

data ScoredLink = ScoredLink
    { sl_link :: Link
    , sl_score :: Double
    }

instance Eq ScoredLink where
    (==) ScoredLink{sl_score = score1} ScoredLink{sl_score = score2} = score1 == score2

instance Ord ScoredLink where
    compare ScoredLink{sl_score = score1} ScoredLink{sl_score = score2} = compare score1 score2
    (<=) ScoredLink{sl_score = score1} ScoredLink{sl_score = score2} = score1 <= score2

data NGramCrawler = NGramCrawler
    { ngc_startUrl :: URL
    , ngc_endUrl :: URL
    -- `Nothing` if we haven't looked at any pages yet.
    , ngc_pageData :: Maybe PageData
    -- The smoothed n-gram model we build from the destination page.
    , ngc_endUrlModel :: NGramModel
    -- A list of `maxIndexedModels` of n-gram models to be used on link text.
    -- The first is a 1-gram model, the second is a 2-gram model, etc.
    , ngc_indexedModels :: [NGramModel]
    -- A cache of page data for encountered URLs.
    , ngc_urlPageDataCache :: HashMap URL PageData
    -- A set of which URLs we have already visited.
    , ngc_visitedUrls :: HashSet URL
    }

instance MemSize PageData where
    memSize PageData{pd_page = page, pd_links = links, pd_score = score} = memSize page + memSize links + memSize score

instance MemSize NGramCrawler where
    memSize NGramCrawler
        { ngc_startUrl = startUrl
        , ngc_endUrl = endUrl
        , ngc_pageData = pageData
        , ngc_endUrlModel = endUrlModel
        , ngc_indexedModels = indexedModels
        , ngc_urlPageDataCache = urlPageDataCache
        , ngc_visitedUrls = visitedUrls
        } = memSize startUrl +
            memSize endUrl +
            memSize pageData +
            memSize endUrlModel +
            memSize indexedModels +
            memSize urlPageDataCache +
            memSize visitedUrls

instance Crawler NGramCrawler where
    makeCrawler startUrl endUrl = do
        endUrlText <- getUrlContentText endUrl
        return $ NGramCrawler
            { ngc_startUrl = startUrl
            , ngc_endUrl = endUrl
            , ngc_pageData = Nothing
            , ngc_endUrlModel = makeModel n True endUrlText
            -- Create `maxIndexedModels` of n-gram models to be used on link text.
            , ngc_indexedModels = map (flip (`makeModel` False) endUrlText) [1..maxIndexedModels]
            , ngc_urlPageDataCache = M.empty
            , ngc_visitedUrls = S.singleton startUrl
            }

    nextPage crawler =
        case ngc_pageData crawler of
            Just pageData -> do
                -- If we have a current page, try to get the next page from it.
                (nextPageData, newCrawlerWithoutPageData) <- getNextPageData pageData crawler
                let newCrawler = newCrawlerWithoutPageData{ngc_pageData = Just nextPageData}
                let newPage = pd_page nextPageData
                return (newCrawler, newPage)
            Nothing -> do
                -- If we don't have a current page (i.e. this is the first call
                -- to `nextPage`), just parse the start URL and return.
                (pageData, crawler2) <- getPageData crawler Link{l_text = Nothing, l_url = ngc_startUrl crawler}
                let crawler3 = crawler2{ngc_pageData = Just pageData}
                let newPage = pd_page pageData
                return (crawler3, newPage)

-- Get the next page, given a current page and a crawler. Return the next
-- page's data, as well as a new crawler with relevant information filled in.
getNextPageData :: PageData -> NGramCrawler -> IO (PageData, NGramCrawler)
getNextPageData pageData crawler = do
    (link, crawler2) <- getNextPage crawler pageData
    (nextPageData, crawler3) <- getPageData crawler2 link
    return (nextPageData, crawler3)

-- Get the data for a page given the link used to get to the page. Uses a
-- cached value if the link has been requested before.
getPageData :: NGramCrawler -> Link -> IO (PageData, NGramCrawler)
getPageData crawler link = do
    case M.lookup url $ ngc_urlPageDataCache crawler of
        -- If we've already cached this URL, return the cached page data.
        Just pageData -> return (pageData, crawler)
        Nothing -> do
            let scraper = do
                    title <- scrapeTitle
                    links <- scrapeLinks
                    score <- scrapeScore crawler links
                    title `seq` links `seq` score `seq` return (title, links, score)
            (title, links, score) <- scrapeURL (fullUrl url) scraper >>= convertMaybe url

            let page = Page{p_title = title, p_link = link}
            let pageData = PageData{pd_page = page, pd_links = links, pd_score = score}

            let newUrlPageData = M.insert url pageData (ngc_urlPageDataCache crawler)
            return (pageData, crawler{ngc_urlPageDataCache = newUrlPageData})
    where
        url = l_url link

-- Calculate the next page given a crawler and a current page data. Return the
-- link to the next page as well as an updated crawler.
getNextPage :: NGramCrawler -> PageData -> IO (Link, NGramCrawler)
getNextPage crawler pageData = do
    let topLinks = getTopLinks crawler pageData
    (scoredLinks, newCrawler) <- dispatchToLinks crawler (lookahead - 1) topLinks
    let maxScoreLink = sl_link $ maximum scoredLinks

    let newVisitedUrls = S.insert (l_url maxScoreLink) (ngc_visitedUrls newCrawler)
    return (maxScoreLink, newCrawler{ngc_visitedUrls = newVisitedUrls})

-- Dispatch calls to `getBestScoreWithLookahead` to the list of links, properly
-- running on separate threads and merging the new crawlers.
dispatchToLinks :: NGramCrawler -> Word -> [Link] -> IO ([ScoredLink], NGramCrawler)
dispatchToLinks crawler _ [] = return ([], crawler)
dispatchToLinks crawler depth (firstLink : restLinks) = do
    let firstLinkIo = getBestScoreWithLookahead crawler depth firstLink
    -- Start this link on a thread before continuing on to the rest of the links.
    firstLinkMVar <- runIoOnThread firstLinkIo
    -- Recursively call on the rest of the links. This call will only return
    -- once those links' threads have completed.
    (restScoredLinks, restLinksCrawler) <- dispatchToLinks crawler depth restLinks

    -- Now wait for the first link's thread.
    (firstLinkScore, firstLinkCrawler) <- takeMVar firstLinkMVar
    let firstScoredLink = ScoredLink{sl_link = firstLink, sl_score = firstLinkScore}
    let newCrawler = mergeCrawlerCaches firstLinkCrawler restLinksCrawler

    return (firstScoredLink : restScoredLinks, newCrawler)

mergeCrawlerCaches :: NGramCrawler -> NGramCrawler -> NGramCrawler
mergeCrawlerCaches crawler1 crawler2 =
    crawler1{ngc_urlPageDataCache = M.union (ngc_urlPageDataCache crawler1) (ngc_urlPageDataCache crawler2)}

-- Get the best score under the given link using the given current depth.
-- Returns an updated crawler.
getBestScoreWithLookahead :: NGramCrawler -> Word -> Link -> IO (Double, NGramCrawler)
getBestScoreWithLookahead crawler 0 link = do
    (pageData, crawler2) <- getPageData crawler link
    return (pd_score pageData, crawler2)

getBestScoreWithLookahead crawler depth link = do
    (pageData, crawler2) <- getPageData crawler link
    let topLinks = getTopLinks crawler2 pageData

    -- Get the scores of links from this page.
    (scoredLinks, crawler3) <- dispatchToLinks crawler2 (depth - 1) topLinks

    let scoredLinksIncludingThis = ScoredLink{sl_link = link, sl_score = pd_score pageData} : scoredLinks
    let maxScore = sl_score $ maximum scoredLinksIncludingThis

    return (maxScore, crawler3)

-- Get the top links on the given page based on their link text.
getTopLinks :: NGramCrawler -> PageData -> [Link]
getTopLinks crawler pageData =
    topLinks
    where
        links = pd_links pageData

        scoredLinkTexts = map
                (\link -> ScoredLink{sl_link = link, sl_score = scoreLinkText crawler link})
                links
        -- Sort with the highest scores at the beginning.
        sortedScoredLinkTexts = sortBy (flip compare) scoredLinkTexts
        sortedLinks = map sl_link sortedScoredLinkTexts
        -- Don't consider URLs we've already visited.
        filteredLinks = filter (not . (`S.member` ngc_visitedUrls crawler) . l_url) sortedLinks
        topLinks = take (fromIntegral maxTopLinks) filteredLinks

-- Get the content of the page identified by a given URL.
getUrlContentText :: URL -> IO Text
getUrlContentText url =
    scrapeURL (fullUrl url) scrapeContentText >>= convertMaybe url

-- Score the text of the given link.
scoreLinkText :: NGramCrawler -> Link -> Double
scoreLinkText crawler Link{l_text = linkText, l_url = url}
    -- If the URL is the end URL, we definitely want to choose it, so score it
    -- infinity.
    | url == ngc_endUrl crawler = infinity
    -- If we are able to score the link text using one of our indexed models,
    -- do that.
    | numWordsInLinkText > 0 && numWordsInLinkText <= maxIndexedModels = scoreText indexedModel strippedLinkText
    -- If the link text has too many words to be scored with our indexed
    -- models, return a 0 score.
    | otherwise = 0
    where
        -- `linkText` shouldn't be `Nothing` here. If it is, throw.
        strippedLinkText = T.strip $ fromJust linkText
        numWordsInLinkText = fromIntegral $ length $ T.words strippedLinkText
        indexedModel = ngc_indexedModels crawler !! fromIntegral (numWordsInLinkText - 1)

-- Score the contents of the link.
scrapeScore :: NGramCrawler -> [Link] -> Scraper Text Double
scrapeScore crawler links =
    scraper
    where
        model = ngc_endUrlModel crawler
        scraper = do
            if ngc_endUrl crawler `elem` map l_url links then
                -- If the target page contains a link to the end URL,
                -- it should have an infinity score.
                return infinity
            else do
                -- Otherwise just score the page's content.
                contentText <- scrapeContentText
                return $ scoreText model contentText

runIoOnThread :: IO a -> IO (MVar a)
runIoOnThread io = do
    mVar <- newEmptyMVar
    let thread = do
            result <- io
            putMVar mVar result
    _ <- forkIO thread
    return mVar

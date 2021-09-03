module BacktrackingCrawler
    ( makeCrawler
    , nextPage
    , BacktrackingCrawler
    ) where

import Crawler (Crawler, makeCrawler, nextPage)
import MemSize (MemSize, memSize)
import NGramModel (makeModel, NGramModel, scoreText)
import Page (convertMaybe, fullUrl, getUrlContentText, Link(..), Page(..), scrapeContentText, scrapeLinks, scrapeTitle)

import Data.HashMap.Strict (HashMap); import qualified Data.HashMap.Strict as M
import Data.HashSet (HashSet); import qualified Data.HashSet as S
import Data.Heap (MaxHeap); import qualified Data.Heap as H;
import Data.List (foldl', sortBy)
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

pageDataToScoredLink :: PageData -> ScoredLink
pageDataToScoredLink pageData =
    ScoredLink{sl_link = link, sl_score = pd_score pageData}
    where
        link = (p_link . pd_page) pageData

instance Eq ScoredLink where
    (==) ScoredLink{sl_score = score1} ScoredLink{sl_score = score2} = score1 == score2

instance Ord ScoredLink where
    compare ScoredLink{sl_score = score1} ScoredLink{sl_score = score2} = compare score1 score2
    (<=) ScoredLink{sl_score = score1} ScoredLink{sl_score = score2} = score1 <= score2

data BacktrackingCrawler = BacktrackingCrawler
    { btc_startUrl :: URL
    , btc_endUrl :: URL
    -- `Nothing` if we haven't looked at any pages yet.
    , btc_pageData :: Maybe PageData
    -- The smoothed n-gram model we build from the destination page.
    , btc_endUrlModel :: NGramModel
    -- A list of `maxIndexedModels` of n-gram models to be used on link text.
    -- The first is a 1-gram model, the second is a 2-gram model, etc.
    , btc_indexedModels :: [NGramModel]
    -- A cache of page data for encountered URLs.
    , btc_urlPageDataCache :: HashMap URL PageData
    -- A set of which URLs we have already visited.
    , btc_visitedUrls :: HashSet URL
    -- A heap with the highest scoring links on top.
    , btc_linkHeap :: MaxHeap ScoredLink
    }

instance MemSize PageData where
    memSize PageData{pd_page = page, pd_links = links, pd_score = score} = memSize page + memSize links + memSize score

instance MemSize BacktrackingCrawler where
    memSize BacktrackingCrawler
        { btc_startUrl = startUrl
        , btc_endUrl = endUrl
        , btc_pageData = pageData
        , btc_endUrlModel = endUrlModel
        , btc_indexedModels = indexedModels
        , btc_urlPageDataCache = urlPageDataCache
        , btc_visitedUrls = visitedUrls
        , btc_linkHeap = _
        } = memSize startUrl +
            memSize endUrl +
            memSize pageData +
            memSize endUrlModel +
            memSize indexedModels +
            memSize urlPageDataCache +
            memSize visitedUrls
            -- memSize linkHeap -- calculating the heap size is a pain because of how instance declarations work with type synonyms.

instance Crawler BacktrackingCrawler where
    makeCrawler startUrl endUrl = do
        endUrlText <- getUrlContentText endUrl
        return $ BacktrackingCrawler
            { btc_startUrl = startUrl
            , btc_endUrl = endUrl
            , btc_pageData = Nothing
            , btc_endUrlModel = makeModel n True endUrlText
            -- Create `maxIndexedModels` of n-gram models to be used on link text.
            , btc_indexedModels = map (flip (`makeModel` False) endUrlText) [1..maxIndexedModels]
            , btc_urlPageDataCache = M.empty
            , btc_visitedUrls = S.singleton startUrl
            , btc_linkHeap = H.empty
            }

    nextPage crawler =
        case btc_pageData crawler of
            Just pageData -> do
                -- If we have a current page, add its neighbors to our max heap.
                crawler2 <- addNeighborsToHeap crawler pageData
                (nextPageData, crawler3) <- getNextPageData crawler2
                let crawler4 = crawler3{btc_pageData = Just nextPageData}
                let newPage = pd_page nextPageData
                return (crawler4, newPage)
            Nothing -> do
                -- If we don't have a current page (i.e. this is the first call
                -- to `nextPage`), just parse the start URL and return.
                (pageData, crawler2) <- getPageData crawler Link{l_text = Nothing, l_url = btc_startUrl crawler}
                let crawler3 = crawler2{btc_pageData = Just pageData}
                let newPage = pd_page pageData
                return (crawler3, newPage)

getNextPageData :: BacktrackingCrawler -> IO (PageData, BacktrackingCrawler)
getNextPageData crawler = do
    let (link, crawler2) = getNextPage crawler
    getPageData crawler2 link

-- Get the data for a page given the link used to get to the page. Uses a
-- cached value if the link has been requested before.
getPageData :: BacktrackingCrawler -> Link -> IO (PageData, BacktrackingCrawler)
getPageData crawler link = do
    case M.lookup url $ btc_urlPageDataCache crawler of
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

            let newUrlPageData = M.insert url pageData (btc_urlPageDataCache crawler)
            return (pageData, crawler{btc_urlPageDataCache = newUrlPageData})
    where
        url = l_url link

-- Call getPageData on a list of links, returning the resulting list of page
-- datas and a new crawler.
getPageDatas :: BacktrackingCrawler -> [Link] -> IO ([PageData], BacktrackingCrawler)
getPageDatas crawler [] = return ([], crawler)
getPageDatas crawler (firstLink:restLinks) = do
    (firstPageData, crawler2) <- getPageData crawler firstLink
    (restPageDatas, crawler3) <- getPageDatas crawler2 restLinks
    return (firstPageData : restPageDatas, crawler3)

getNextPage :: BacktrackingCrawler -> (Link, BacktrackingCrawler)
getNextPage crawler =
    (nextLink, crawler2{btc_visitedUrls = newVisitedUrls})
    where
        (nextLink, crawler2) = popNextLink crawler
        newVisitedUrls = S.insert (l_url nextLink) (btc_visitedUrls crawler2)

-- Pop the link with the highest score off the crawler's heap, returning the
-- link and a new crawler.
popNextLink :: BacktrackingCrawler -> (Link, BacktrackingCrawler)
popNextLink crawler =
    (sl_link scoredLink, crawler{btc_linkHeap = newHeap})
    where
        (scoredLink, newHeap) = fromJust $ H.view (btc_linkHeap crawler)

-- Add the neighbors of the current page to the crawler's heap.
addNeighborsToHeap :: BacktrackingCrawler -> PageData -> IO BacktrackingCrawler
addNeighborsToHeap crawler pageData = do
    let topLinks = getTopLinks crawler pageData
    (topLinkPageDatas, crawler2) <- getPageDatas crawler topLinks
    let topScoredLinks = map pageDataToScoredLink topLinkPageDatas
    let newHeap = foldl' addToHeap (btc_linkHeap crawler2) topScoredLinks
    return crawler2{btc_linkHeap = newHeap}
    where
        addToHeap heap scoredLink = H.insert scoredLink heap

-- Get the top links on the given page based on their link text.
getTopLinks :: BacktrackingCrawler -> PageData -> [Link]
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
        filteredLinks = filter (not . (`S.member` btc_visitedUrls crawler) . l_url) sortedLinks
        topLinks = take (fromIntegral maxTopLinks) filteredLinks

-- Score the text of the given link.
scoreLinkText :: BacktrackingCrawler -> Link -> Double
scoreLinkText crawler Link{l_text = linkText, l_url = url}
    -- If the URL is the end URL, we definitely want to choose it, so score it
    -- infinity.
    | url == btc_endUrl crawler = infinity
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
        indexedModel = btc_indexedModels crawler !! fromIntegral (numWordsInLinkText - 1)

-- Score the contents of the link.
scrapeScore :: BacktrackingCrawler -> [Link] -> Scraper Text Double
scrapeScore crawler links =
    scraper
    where
        model = btc_endUrlModel crawler
        scraper = do
            if btc_endUrl crawler `elem` map l_url links then
                -- If the target page contains a link to the end URL,
                -- it should have an infinity score.
                return infinity
            else do
                -- Otherwise just score the page's content.
                contentText <- scrapeContentText
                return $ scoreText model contentText

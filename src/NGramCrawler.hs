module NGramCrawler
    ( NGramCrawler
    , makeCrawler
    , nextPage
    ) where

import Crawler (Crawler, makeCrawler, nextPage)
import NGramModel (NGramModel, makeModel, scoreText)
import Page (Page(..), fullUrl, scrapeTitle, scrapeLinks, scrapeContentText, convertMaybe)

import Data.Foldable (maximumBy)
import Text.HTML.Scalpel (URL, scrapeURL)

data PageData = PageData
    { pd_page :: Page
    , pd_linkScores :: [((String, URL), Double)]
    }

data NGramCrawler = NGramCrawler
    { ngc_startUrl :: URL
    , ngc_pageData :: Maybe PageData
    , ngc_destinationModel :: NGramModel
    }

instance Crawler NGramCrawler where
    makeCrawler startUrl endUrl = do
        destinationModel <- makeModelFromUrl endUrl
        return $ NGramCrawler
            { ngc_startUrl = startUrl
            , ngc_pageData = Nothing
            , ngc_destinationModel = destinationModel
            }

    nextPage crawler =
        case ngc_pageData crawler of
            Just pageData -> do
                nextPageData <- getNextPageData pageData $ ngc_destinationModel crawler
                let newCrawler = crawler { ngc_pageData = Just nextPageData }
                let newPage = pd_page nextPageData
                return (newCrawler, newPage)
            Nothing -> do
                pageData <- getPageData Nothing (ngc_startUrl crawler) (ngc_destinationModel crawler)
                let newCrawler = crawler { ngc_pageData = Just pageData }
                let newPage = pd_page pageData
                return (newCrawler, newPage)

getNextPageData :: PageData -> NGramModel -> IO PageData
getNextPageData pageData model = do
    let (sourceLinkText, url) = getNextPage pageData
    getPageData (Just sourceLinkText) url model

getPageData :: Maybe String -> URL -> NGramModel -> IO PageData
getPageData sourceLinkText url model = do
    let scraper = do
            title <- scrapeTitle
            links <- scrapeLinks
            let linkScoresIo = listIoToIoList $ map (scoreLink model) links
            return (title, linkScoresIo)
    (title, linkScoresIo) <- scrapeURL (fullUrl url) scraper >>= convertMaybe url
    linkScores <- linkScoresIo
    let page = Page { p_title = title, p_url = url, p_sourceLinkText = sourceLinkText }
    return $ PageData { pd_page = page, pd_linkScores = linkScores }

getNextPage :: PageData -> (String, URL)
getNextPage pageData =
    link
    where
        scores = pd_linkScores pageData
        maxScoreLink = maximumBy compareLinks scores
        (link, _) = maxScoreLink

n :: Word
n = 1

makeModelFromUrl :: URL -> IO NGramModel
makeModelFromUrl url =
    scrapeURL (fullUrl url) scraper >>= convertMaybe url
    where
        scraper = makeModel n <$> scrapeContentText

scoreLink :: NGramModel -> (String, URL) -> IO ((String, URL), Double)
scoreLink model (sourceLinkText, url) =
    scrapeURL (fullUrl url) scraper >>= convertMaybe url
    where
        scraper = do
            contentText <- scrapeContentText
            return ((sourceLinkText, url), scoreText model contentText)

compareLinks :: ((String, URL), Double) -> ((String, URL), Double) -> Ordering
compareLinks (_, score1) (_, score2) = compare score1 score2

listIoToIoList :: [IO a] -> IO [a]
listIoToIoList (first : rest) = do
    firstUnwrapped <- first
    restUnwrapped <- listIoToIoList rest
    return $ firstUnwrapped : restUnwrapped
listIoToIoList [] = return []

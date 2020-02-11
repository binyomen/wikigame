module NGramCrawler
    ( NGramCrawler
    , makeCrawler
    , nextPage
    ) where

import Crawler (Crawler, makeCrawler, nextPage)
import NGramModel (NGramModel, makeModel)
import Page (Page(..), fullUrl, scrapeTitle, scrapeLinks, convertMaybe)

import Text.HTML.Scalpel (URL, scrapeURL)

data PageData = PageData
    { page :: Page
    , links :: [(String, URL)]
    }

data NGramCrawler = NGramCrawler
    { startUrl :: URL
    , pageData :: Maybe PageData
    }

instance Crawler NGramCrawler where
    makeCrawler startUrl = NGramCrawler { startUrl = startUrl, pageData = Nothing }

    nextPage crawler =
        return (crawler, Page { title = "title", url = "url", sourceLinkText = Nothing })

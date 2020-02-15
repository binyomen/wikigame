module NGramCrawler
    ( NGramCrawler
    , makeCrawler
    , nextPage
    ) where

import Crawler (Crawler, makeCrawler, nextPage)
import NGramModel ()
import Page (Page(..))

import Text.HTML.Scalpel (URL)

data PageData = PageData
    { pd_page :: Page
    , pd_links :: [(String, URL)]
    }

data NGramCrawler = NGramCrawler
    { ngc_startUrl :: URL
    , ngc_pageData :: Maybe PageData
    }

instance Crawler NGramCrawler where
    makeCrawler startUrl = NGramCrawler { ngc_startUrl = startUrl, ngc_pageData = Nothing }

    nextPage crawler =
        return (crawler, Page { p_title = "title", p_url = "url", p_sourceLinkText = Nothing })

module Crawler
    ( Crawler
    , makeCrawler
    , nextPage
    ) where

import Page (Page)

import Text.HTML.Scalpel (URL)

class Crawler a where
    makeCrawler :: URL -> a
    nextPage :: a -> IO (a, Page)

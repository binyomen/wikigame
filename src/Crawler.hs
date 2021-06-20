module Crawler
    ( Crawler
    , makeCrawler
    , nextPage
    ) where

import Page (Page)

import Text.HTML.Scalpel (URL)

-- A type class defining a web crawler which can play the Wikipedia game.
class Crawler a where
    makeCrawler :: URL -> URL -> IO a
    nextPage :: a -> IO (a, Page)

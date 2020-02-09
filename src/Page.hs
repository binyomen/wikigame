{-# LANGUAGE OverloadedStrings #-}

module Page
    ( title
    , scrapePage
    ) where

import Data.List (isPrefixOf)
import Text.HTML.Scalpel
    ( Scraper
    , URL
    , scrapeURL
    , chroot
    , attrs
    , match
    , text
    , (@:)
    , (@=)
    )

data Page = Page
    { title :: String
    , links :: [URL]
    }

testing :: String
testing = "a"

scrapePage :: URL -> IO Page
scrapePage url =
    scrapeURL url scraper >>= convertMaybe url
    where
        scraper = do
            title <- scrapeTitle
            links <- scrapeLinks
            return $ Page {title = title, links = links}

scrapeTitle :: Scraper String String
scrapeTitle = text $ "h1" @: ["id" @= "firstHeading"]

scrapeLinks :: Scraper String [String]
scrapeLinks =
    chroot ("div" @: ["id" @= "mw-content-text"]) linkScraper
    where
        linkScraper = attrs "href" $ "a" @: [match isWikipediaLink]

isWikipediaLink :: String -> String -> Bool
isWikipediaLink key value =
    case key of
        "href" -> "/wiki/" `isPrefixOf` value
        _ -> False

convertMaybe :: URL -> Maybe a -> IO a
convertMaybe _ (Just v) = return v
convertMaybe url Nothing = fail $ "Could not read page " ++ url

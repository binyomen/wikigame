{-# LANGUAGE OverloadedStrings #-}

module Page
    ( convertMaybe
    , fullUrl
    , Link(..)
    , Page(..)
    , scrapeContentText
    , scrapeLinks
    , scrapeTitle
    ) where

import MemSize (MemSize, memSize)

import Data.List (isPrefixOf)
import Data.Maybe (fromJust)
import Data.Text (Text); import qualified Data.Text as T
import Text.HTML.Scalpel
    ( (//)
    , (@:)
    , (@=)
    , anySelector
    , attr
    , chroots
    , match
    , Scraper
    , text
    , URL
    )

data Link = Link
    -- l_text is `Nothing` if this is the first page.
    { l_text :: Maybe Text
    , l_url :: URL
    }

instance MemSize Link where
    memSize Link{l_text = t, l_url = url} = memSize t + memSize url

data Page = Page
    { p_title :: Text
    , p_link :: Link
    }

instance MemSize Page where
    memSize Page{p_title = title, p_link = link} = memSize title + memSize link

fullUrl :: URL -> URL
fullUrl url = "https://en.wikipedia.org/wiki/" ++ url

scrapeTitle :: Scraper Text Text
scrapeTitle = text $ "h1" @: ["id" @= "firstHeading"]

scrapeLinks :: Scraper Text [Link]
scrapeLinks =
    chroots
        (("div" @: ["id" @= "mw-content-text"]) // ("a" @: [match isWikipediaLink]))
        linkScraper
    where
        linkScraper :: Scraper Text Link
        linkScraper = do
            linkText <- text anySelector
            linkUrl <- attr "href" anySelector
            let strippedUrl = T.unpack $ fromJust $ T.stripPrefix "/wiki/" linkUrl
            linkText `seq` strippedUrl `seq` return Link{l_text = Just linkText, l_url = strippedUrl}

scrapeContentText :: Scraper Text Text
scrapeContentText =
    text $ "div" @: ["id" @= "mw-content-text"]

isWikipediaLink :: String -> String -> Bool
isWikipediaLink key value =
    case key of
        "href" ->
            "/wiki/" `isPrefixOf` value &&
            notElem ':' value &&
            notElem '#' value
        _ -> False

convertMaybe :: URL -> Maybe a -> IO a
convertMaybe _ (Just v) = return v
convertMaybe url Nothing = fail $ "Could not read page " ++ url

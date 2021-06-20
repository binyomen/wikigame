{-# LANGUAGE OverloadedStrings #-}

module Page
    ( Link(..)
    , Page(..)
    , fullUrl
    , scrapeTitle
    , scrapeLinks
    , scrapeContentText
    , convertMaybe
    ) where

import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (fromJust)
import Text.HTML.Scalpel
    ( Scraper
    , URL
    , anySelector
    , chroots
    , attr
    , match
    , text
    , (//)
    , (@:)
    , (@=)
    )

data Link = Link
    -- l_text is `Nothing` if this is the first page.
    { l_text :: Maybe String
    , l_url :: URL
    }

data Page = Page
    { p_title :: String
    , p_link :: Link
    }

fullUrl :: URL -> URL
fullUrl url = "https://en.wikipedia.org/wiki/" ++ url

scrapeTitle :: Scraper String String
scrapeTitle = text $ "h1" @: ["id" @= "firstHeading"]

scrapeLinks :: Scraper String [Link]
scrapeLinks =
    chroots
        (("div" @: ["id" @= "mw-content-text"]) // ("a" @: [match isWikipediaLink]))
        linkScraper
    where
        linkScraper :: Scraper String Link
        linkScraper = do
            linkText <- text anySelector
            linkUrl <- attr "href" anySelector
            return Link{l_text = Just linkText, l_url = fromJust $ stripPrefix "/wiki/" linkUrl}

scrapeContentText :: Scraper String String
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

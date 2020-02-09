{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( playGame
    ) where

import Text.HTML.Scalpel (URL, scrapeURL, text, (@:), (@=))

couldNotReadPageTitleError :: String
couldNotReadPageTitleError = "Could not read page title"

playGame :: String -> String -> IO ()
playGame startUrl endUrl = do
    startTitle <- getTitle startUrl >>= convertMaybe couldNotReadPageTitleError
    endTitle <- getTitle endUrl >>= convertMaybe couldNotReadPageTitleError
    putStrLn $ startTitle ++ ", " ++ endTitle

getTitle :: URL -> IO (Maybe String)
getTitle url =
    scrapeURL url scraper
    where
        scraper = text $ "h1" @: ["id" @= "firstHeading"]

convertMaybe :: String -> Maybe a -> IO a
convertMaybe _ (Just v) = return v
convertMaybe errMsg Nothing = fail errMsg

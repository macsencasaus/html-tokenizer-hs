{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List (find)
import Network.HTTP.Simple
import Tokenizer

isStartTagTitle :: Token -> Bool
isStartTagTitle (StartTagToken "title" _) = True
isStartTagTitle _ = False

isText :: Token -> Bool
isText (TextToken _) = True
isText _ = False

main :: IO ()
main = do
  body <- getResponseBody <$> httpBS "https://owenshadburne.github.io"

  case tokenizer body of
    Right ts -> do
      let titleToken = find isText $ dropWhile (not . isStartTagTitle) ts
      case titleToken of
        Just (TextToken title) -> print title
        _ -> print "title not found"
    Left err -> print err

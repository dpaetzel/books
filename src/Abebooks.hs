{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


module Abebooks where


import Protolude


import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy.Encoding as LE
import Data.Text
import Text.XML.Light.Input
import Text.XML.Light.Proc
import Text.XML.Light.Types


import Book


abebooksURL :: ISBN -> Text
abebooksURL isbn
  = "https://www.abebooks.com/book-search/isbn/" <> isbn <> "/"


-- used for experimenting
abebooksData :: IO B.ByteString
abebooksData = B.readFile "abebooks.html"


-- NOTE goodread's API returns only one author (the first one)
-- NOTE must only pull at most once per second
-- NOTE full XML paths: GoodreadsResponse.search.results.work.best_book.{title,author.name}
fromAbebooks :: ISBN -> B.ByteString -> Either Text Book
fromAbebooks isbn s
  = do
    a <- author
    t <- title
    return $ Book [a] isbn t "AbeBooks"
  where
    itemProp = Attr (QName "itemprop" Nothing Nothing)
    nameQuery = QName "name" Nothing Nothing
    contentQuery = QName "content" Nothing Nothing
    title
      = case parseXMLDoc s of
        Nothing -> Left "Parsing XML document failed"
        Just doc -> case
          filterElement (\e -> itemProp "name" `Protolude.elem` elAttribs e) doc
          of
            Nothing -> Left "No name property found"
            Just doc -> case findAttr contentQuery doc of
              Nothing -> Left "No content attribute found in name property"
              Just doc -> Right . pack $ doc
    author
      = case parseXMLDoc s of
        Nothing -> Left "Parsing XML document failed"
        Just doc -> case
          filterElement (\e -> itemProp "author" `Protolude.elem` elAttribs e) doc
          of
            Nothing -> Left "No author property found"
            Just doc -> case findAttr contentQuery doc of
              Nothing -> Left "No content attribute found in author property"
              Just doc -> Right . pack $ doc

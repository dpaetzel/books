{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}


module Goodreads where


import Protolude


import qualified Data.ByteString.Lazy as B
import Data.Text
import Text.XML.Light.Input
import Text.XML.Light.Proc
import Text.XML.Light.Types


import Book


goodreadsURL :: Text -> ISBN -> Text
goodreadsURL apiKey isbn
  = "https://www.goodreads.com/search/index.xml?key=" <> apiKey <> "&q=" <> isbn


-- used for experimenting
goodreadsData :: Text
goodreadsData = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<GoodreadsResponse>\n  <Request>\n    <authentication>true</authentication>\n      <key><![CDATA[rEhjE7tbEX8dVlK0LiMHQ]]></key>\n    <method><![CDATA[search_index]]></method>\n  </Request>\n  <search>\n  <query><![CDATA[9780980200447]]></query>\n    <results-start>1</results-start>\n    <results-end>1</results-end>\n    <total-results>1</total-results>\n    <source>Goodreads</source>\n    <query-time-seconds>0.01</query-time-seconds>\n    <results>\n        <work>\n  <id type=\"integer\">6571544</id>\n  <books_count type=\"integer\">2</books_count>\n  <ratings_count type=\"integer\">36</ratings_count>\n  <text_reviews_count type=\"integer\">18</text_reviews_count>\n  <original_publication_year type=\"integer\">2009</original_publication_year>\n  <original_publication_month type=\"integer\">3</original_publication_month>\n  <original_publication_day type=\"integer\">15</original_publication_day>\n  <average_rating>3.61</average_rating>\n  <best_book type=\"Book\">\n    <id type=\"integer\">6383507</id>\n    <title>Slow Reading</title>\n    <author>\n      <id type=\"integer\">2892701</id>\n      <name>John Miedema</name>\n    </author>\n    <image_url>https://s.gr-assets.com/assets/nophoto/book/111x148-bcc042a9c91a29c1d680899eff700a03.png</image_url>\n    <small_image_url>https://s.gr-assets.com/assets/nophoto/book/50x75-a91bf249278a81aabab721ef782c4a74.png</small_image_url>\n  </best_book>\n</work>\n\n    </results>\n</search>\n\n</GoodreadsResponse>"


-- NOTE goodread's API returns only one author (the first one)
-- NOTE must only pull at most once per second
-- NOTE full XML paths: GoodreadsResponse.search.results.work.best_book.{title,author.name}
fromGoodreads :: ISBN -> B.ByteString -> Either Text Book
fromGoodreads isbn s
  = do
    a <- author
    t <- title
    return $ Book [a] isbn t Nothing
  where
    authorQuery = QName "author" Nothing Nothing
    nameQuery = QName "name" Nothing Nothing
    author :: Either Text Text
    author
      = case parseXMLDoc s of
        Nothing -> Left "Parsing XML document failed"
        Just doc -> case findElement authorQuery doc of
          Nothing ->  Left "No author tag found"
          Just doc -> case findElement nameQuery doc of
            Nothing ->  Left "No name tag for author found"
            Just doc -> Right . pack . strContent $ doc
    titleQuery = QName "title" Nothing Nothing
    title :: Either Text Text
    title
      = case parseXMLDoc s of
        Nothing -> Left "Parsing XML document failed"
        Just doc -> case findElement titleQuery doc of
          Nothing ->  Left "No title tag found"
          Just doc -> Right . pack . strContent $ doc



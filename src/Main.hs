{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}


-- https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/json


module Main where


import Protolude


import Control.Arrow (left)
import Control.Concurrent (threadDelay)
import Data.Text
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)


import Book
import Goodreads
import Goodreads.APIKey
import OpenLibrary


fetch
  :: (ISBN -> B.ByteString -> Either Text Book)
  -> (ISBN -> Text)
  -> ISBN
  -> IO (Either (Text, ISBN) Book)
fetch parseIt createUrl isbn
  = fmap (left (, isbn) . parseIt isbn) . simpleHttp . unpack . createUrl $ isbn


isbn1 = "9780980200447" :: Text
isbn2 = "9783442472444" :: Text


fetchAll :: ISBN -> IO (Either (Text, ISBN) Book)
fetchAll isbn
  = do
    threadDelay 1500000
    bookE <- fetch fromOpenLibrary openLibraryURL isbn
    case bookE of
      Left err -> do
        putStrLn ("OpenLibrary didn't work, trying Goodreads …" :: Text)
        fetch fromGoodreads (goodreadsURL apiKey) isbn
      Right b -> return . Right $ b


main
  = do
    b1 <- fetchAll isbn1
    b2 <- fetchAll isbn2
    print b1
    print b2

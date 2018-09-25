{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


module OpenLibrary where


import Protolude


import Control.Arrow (left)
import Data.Aeson
import Data.Aeson.Types
import Data.Text
import qualified Data.ByteString.Lazy as B


import Book
import OpenLibrary.Author


openLibraryURL :: ISBN -> Text
openLibraryURL isbn
  = "https://openlibrary.org/api/books?format=json&jscmd=data&bibkeys=ISBN:" <> isbn


fromOpenLibrary :: ISBN -> B.ByteString -> Either Text Book
fromOpenLibrary isbn s
  = left pack $ do
    result <- eitherDecode s
    flip parseEither result $ \obj -> do
      obj' <- obj .: ("ISBN:" <> isbn)
      authors <- obj' .: "authors"
      title <- obj' .: "title"
      return $ Book (name <$> authors) isbn title "OpenLibrary"


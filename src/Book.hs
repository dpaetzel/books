{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


module Book where


import Protolude hiding (intercalate)


import Data.Text


type ISBN = Text


data Book
  = Book
  { authors :: [Text]
  , isbn :: ISBN
  , title :: Text
  , source :: Text
  }
  deriving (Show)


csv :: Book -> Text
csv (Book authors isbn title source)
  = isbn `mappend` "," `mappend`
    quote title `mappend` "," `mappend`
    quote (intercalate " & " authors) `mappend` "," `mappend`
    source
  where
    quote s = "\"" `mappend` s `mappend` "\""

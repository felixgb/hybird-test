{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Server
    ( serve
    , readPointsCsv
    ) where

import Web.Scotty
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import Data.Csv
import Data.Aeson (ToJSON)
import qualified Data.Vector as V
import GHC.Generics

data Point = Point
  { id :: !Int
  , x :: !Int
  , y :: !Int
  , z :: !Int
  , color :: Maybe Text
  } deriving (Show, Generic)

instance FromNamedRecord Point where
  parseNamedRecord r = Point 
    <$> r .: "Point"
    <*> r .: "x"
    <*> r .: "y"
    <*> r .: "z"
    <*> r .: "Colour"

instance ToJSON Point

readPointsCsv :: FilePath -> IO [Point]
readPointsCsv name = do
  csvData <- BL.readFile name
  case decodeByName csvData of
    Left err -> error err
    Right (_, v) -> pure $ V.toList v

serve points = scotty 8000 $
  get "/:points" $ do
    json points

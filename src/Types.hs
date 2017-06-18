{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Types (
  -- * Types
    Config(..)
  , ImageUploadRequest(..)
  , ImageUploadResponse(..)
  , UploadedImage(..)
  , RawImage
  , ImageId
  , URL
) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Database.Redis as R
import Data.Aeson
import GHC.Generics

data Config = Config {
  conn :: R.Connection
}

type ImageAction a = IO (Either R.Reply a)

data ImageUploadRequest = ImageUploadRequest {
  images :: [URL]
} deriving (Show, Generic, FromJSON, ToJSON)

data ImageUploadResponse = ImageUploadResponse {
  images :: [UploadedImage]
} deriving (Show, Generic, FromJSON, ToJSON)

data UploadedImage = UploadedImage {
  id :: ImageId,
  originalUrl :: URL,
  proxyUrl :: URL
} deriving (Show, Generic, FromJSON, ToJSON)

type RawImage = BL.ByteString

type ImageId = T.Text

type URL = T.Text
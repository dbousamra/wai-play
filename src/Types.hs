{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Types (
  -- * Types
    Config(..)
  , ImageUploadRequest(..)
  , ImageUploadResponse(..)
  , UploadedImage(..)
  , ImageStatus(..)
  , RawImage
  , ImageId
  , URL
  , Error
) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Database.Redis as R
import Data.Aeson
import GHC.Generics

data Config = Config {
  connection :: R.Connection
}
data ImageUploadRequest = ImageUploadRequest {
  images :: [URL]
} deriving (Show, Generic, FromJSON, ToJSON)

data ImageUploadResponse = ImageUploadResponse {
  images :: [UploadedImage]
} deriving (Show, Generic, FromJSON, ToJSON)

data UploadedImage = UploadedImage {
  id :: ImageId,
  originalUrl :: URL,
  proxyUrl :: URL,
  uploadStatus :: ImageStatus
} deriving (Show, Generic, FromJSON, ToJSON)

data ImageStatus
  = ImageError
  | ImageSuccess
  deriving (Show, Generic, FromJSON, ToJSON)

type RawImage = BS.ByteString

type ImageId = T.Text

type URL = T.Text

type Error = L.Text
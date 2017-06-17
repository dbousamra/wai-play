{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Types (
  -- * Types
  ImageUploadRequest(..)
  -- * Functions
) where

import qualified Data.Text as T
import Data.Aeson
import GHC.Generics

data ImageUploadRequest = ImageUploadRequest {
  images :: [T.Text]
} deriving (Show, Generic, FromJSON, ToJSON)
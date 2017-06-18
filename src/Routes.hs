{-# LANGUAGE OverloadedStrings #-}

module Routes where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import Web.Scotty
import Network.HTTP.Types.Status
import qualified Database.Redis as R
import Control.Monad.IO.Class (liftIO)
import Types
import Api

getImage :: R.Connection -> ScottyM ()
getImage conn = get (capture "/images/:id") $ do
  id <- param "id"
  resp <- liftIO $ getImageById conn id
  case resp of
    Right (Just bytes)  -> do
      setHeader "Content-Type" "image/jpeg"
      raw bytes
    Right (Nothing) -> status notFound404
    Left e -> status internalServerError500 

postImage :: R.Connection -> ScottyM ()
postImage conn = post "/images" $ do
  imagesRequest  <- jsonData
  imagesResponse <- liftIO $ uploadImages conn imagesRequest
  either (const $ status internalServerError500) json imagesResponse

fallback :: ScottyM ()
fallback = notFound $ status notFound404

routes :: R.Connection -> ScottyM ()
routes conn = do
  getImage conn
  postImage conn
  fallback
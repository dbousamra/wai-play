{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DeriveFunctor #-}

module Api (
  -- * Functions
    getImageById
  , uploadImages
) where

import Data.Bifunctor (second)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Data.Semigroup
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import qualified Database.Redis as R
import qualified Network.HTTP.Simple as H
import qualified Control.Concurrent.Async as A
import Types

uploadImages :: R.Connection -> ImageUploadRequest -> ImageAction ImageUploadResponse
uploadImages conn (ImageUploadRequest images) = do
  resps <-  A.mapConcurrently (uploadImage conn) images
  pure $ fmap ImageUploadResponse (sequence resps)

uploadImage :: R.Connection -> URL -> ImageAction UploadedImage
uploadImage conn url = do
  id <- fmap toText nextRandom
  bytes <- downloadImage url
  _ <- putImage conn id bytes
  pure $ Right $ UploadedImage id url ("http://localhost:3000/images/" <> id)

putImage :: R.Connection -> ImageId -> RawImage -> ImageAction ()
putImage conn id bytes = R.runRedis conn $ do
  resp <- R.set (TE.encodeUtf8 id) (BL.toStrict bytes)
  pure $ (second $ const ()) resp

getImageById :: R.Connection -> ImageId -> ImageAction (Maybe RawImage)
getImageById conn id = R.runRedis conn $ do
  resp :: Either R.Reply (Maybe BS.ByteString) <- R.get $ TE.encodeUtf8 id
  pure $ fmap (\x -> fmap (\y -> BL.fromStrict y) x) resp

downloadImage :: URL -> IO RawImage
downloadImage url = do 
  req <- H.parseRequest $ T.unpack url
  resp <- H.httpLbs req
  pure $ H.getResponseBody resp
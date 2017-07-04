{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DeriveFunctor #-}

module Api (
  -- * Functions
    getImageById
  , uploadImages
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Control.Error.Util (hoistEither)
import Data.Bifunctor (first, bimap)
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
import Codec.Picture (decodeImage, encodeJpeg)
import Codec.Picture.Saving (imageToJpg)
import Codec.Picture.Types as P
import qualified Vision.Image as I
import Vision.Image.JuicyPixels
import Types

uploadImages :: R.Connection -> ImageUploadRequest -> IO ImageUploadResponse
uploadImages conn (ImageUploadRequest images) = do
  resps <-  A.mapConcurrently (uploadImage conn) images
  pure $ ImageUploadResponse resps

uploadImage :: R.Connection -> URL -> IO UploadedImage
uploadImage conn url = do
  id <- fmap toText nextRandom
  resp <- runExceptT $ do 
    bytes <- downloadImage url
    putImage conn id bytes
  pure $ case resp of
    Left e  -> UploadedImage id url ("http://localhost:3000/images/" <> id) ImageError
    Right e -> UploadedImage id url ("http://localhost:3000/images/" <> id) ImageSuccess

putImage :: R.Connection -> ImageId -> RawImage -> ExceptT T.Text IO ()
putImage conn id bytes = ExceptT $ R.runRedis conn $ do
  resp <- R.set (TE.encodeUtf8 id) bytes
  pure $ bimap (T.pack . show) (const ()) resp

getImageFromRedis :: R.Connection -> ImageId -> ExceptT T.Text IO (Maybe RawImage)
getImageFromRedis conn id = ExceptT $ R.runRedis conn $ do
  resp <- R.get $ TE.encodeUtf8 id
  pure $ first (T.pack . show) resp

getImageById :: R.Connection -> ImageId -> ExceptT T.Text IO (Maybe RawImage)
getImageById conn id = do
  bs <- getImageFromRedis conn id
  r <- pure $ case bs of 
    Nothing -> pure Nothing
    Just im -> do
      dy <- decodeRawImage im
      pure $ Just im
      -- pure $ Just $ encodeDynamicImage dy
  hoistEither r

downloadImage :: URL -> ExceptT T.Text IO RawImage
downloadImage url = do 
  req <- liftIO $ H.parseRequest $ T.unpack url
  resp <- liftIO $ H.httpLbs req
  pure $ BL.toStrict $ H.getResponseBody resp

decodeRawImage ::  BS.ByteString -> Either T.Text P.DynamicImage
decodeRawImage i = first (T.pack) (decodeImage i)

encodeDynamicImage :: P.DynamicImage -> RawImage
encodeDynamicImage i = BL.toStrict $ imageToJpg 90 i
  
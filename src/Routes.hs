{-# LANGUAGE OverloadedStrings #-}

module Routes(
  -- * Functions
    routes
) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import Control.Monad.Trans.Except (runExceptT)
import Data.Aeson (Value, (.=), object)
import Web.Scotty
import Network.Wai (Middleware)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.HTTP.Types.Status
import qualified Database.Redis as R
import Control.Monad.IO.Class (liftIO)
import Types
import Api

defaultH :: Error -> ActionM ()
defaultH e = do
  status internalServerError500
  json $ object ["error" .= e]

loggingM :: Middleware
loggingM = logStdoutDev

getImage :: R.Connection -> ActionM ()
getImage conn = do
  id <- param "id"
  resp <- liftIO $ runExceptT $ getImageById conn id
  case resp of
    Right (Just bytes)  -> do
      setHeader "Content-Type" "image/jpeg"
      raw $ BL.fromStrict bytes
    Right (Nothing) -> status notFound404
    Left _ -> status internalServerError500 

postImage :: R.Connection -> ActionM ()
postImage conn = do
  imagesRequest  <- jsonData
  imagesResponse <- liftIO $ uploadImages conn imagesRequest
  json imagesResponse

fallback :: ScottyM ()
fallback = notFound $ status notFound404

routes :: Config -> ScottyM ()
routes (Config c) = do
  defaultHandler defaultH
  -- middleware loggingM
  get (capture "/images/:id") $ getImage c
  post "/images" $ postImage c
  fallback
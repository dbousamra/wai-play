{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import Web.Scotty
import Data.Monoid (mconcat)
import Control.Monad.IO.Class
import Types

getImage :: ScottyM ()
getImage = get (capture "/images/:id") $ do
  json ("DOM" :: T.Text)

postImage :: ScottyM ()
postImage = post "/images" $ do
  body <- jsonData :: ActionM ImageUploadRequest
  json (body)

routes :: ScottyM ()
routes = do
  getImage
  postImage

main :: IO ()
main = scotty 3000 $ routes
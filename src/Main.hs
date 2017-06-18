{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Database.Redis as R
import Web.Scotty
import Routes (routes)
import Types

main :: IO ()
main = do 
  c <- getConfig
  scotty 3000 $ routes (conn c)

getConfig :: IO Config
getConfig = do
  c <- getRedisConn
  pure $ Config c

getRedisConn :: IO R.Connection
getRedisConn = R.checkedConnect R.defaultConnectInfo
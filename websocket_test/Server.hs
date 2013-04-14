{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Main where

import System.IO (Handle,hClose)

import Network.WebSockets as WS
import Network

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (forkIO)

import Data.Monoid

main :: IO ()
main = runServer "0.0.0.0" 8000 app

app :: Request -> WebSockets Hybi00 ()
app rq = do
    WS.acceptRequest rq
    liftIO . putStrLn . ("Client version: " ++) =<< WS.getVersion
    WS.sendTextData ("{\"command\":\"test\",\"value\":5}" :: Text)
    sink <- WS.getSink
    forever $ do
        msg :: Text <- WS.receiveData
        liftIO . T.putStrLn $ "You sent " `mappend` msg
        WS.sendTextData ("{\"command\":\"test\",\"value\":8}" :: Text)


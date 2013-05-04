{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, DeriveGeneric, RecordWildCards #-}
module Main where

import ClientProtocol
import ServerProtocol

import Data.Aeson

import Control.Exception as E

import System.IO (Handle,hClose)

import Network.WebSockets as WS
import Network

import Control.Concurrent.STM

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (forkIO)

import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.Wai.Middleware.Static
import Web.Scotty hiding (json)

import Data.Map (Map)
import qualified Data.Map as M

import Data.Monoid

main :: IO ()
main = do
    users <- newTVarIO M.empty
    forkIO $ scotty 3000 $ do
        middleware $ staticPolicy $ mconcat
            [ noDots
            , foldr1 (<|>) (map hasSuffix
                ["html", "js", "css", "jpg", "txt"])
            ] <|> only (zip ["","/"] (repeat "index.html"))
    runServer "0.0.0.0" 8000 (app users)

receiveJSON :: FromJSON a => WebSockets Hybi10 (Maybe a)
receiveJSON = fmap decode WS.receiveData

sendJSON :: ToJSON a => Sink Hybi10 -> a -> IO ()
sendJSON s = WS.sendSink s . DataMessage . Text . encode

app :: TVar (Map String (Sink Hybi10)) -> Request -> WebSockets Hybi10 ()
app users rq = do
    WS.acceptRequest rq
    WS.spawnPingThread 1
    liftIO . putStrLn . ("Client version: " ++) =<< WS.getVersion
    sink <- WS.getSink
    login sink `catchWsError` handleErr sink
  where
    login sink = forever $ do
        msg <- receiveJSON
        case msg of
            Just Connect{..} -> do
                liftIO . atomically $ modifyTVar users (M.insert username sink)
                liftIO $ sendJSON sink (Connected username)
                listen username
            _ -> return ()

    listen username = forever $ do
        msg <- receiveJSON
        case msg of
            Just Send{..} -> do
                users' <- liftIO . atomically $ readTVar users
                forM_ (M.elems users') $ \ sink' ->
                    liftIO (sendJSON sink' (Broadcast username message))
                        `catchWsError` handleErr sink'
            _ -> return ()

    handleErr sink e = liftIO $ do
        putStrLn $ "Got an error: " ++ show e
        atomically $ modifyTVar users (M.filter (/= sink))



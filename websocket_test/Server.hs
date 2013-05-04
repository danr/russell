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

app :: TVar (Map String (Sink Hybi10)) -> Request -> WebSockets Hybi10 ()
app users rq = do
    WS.acceptRequest rq
    WS.spawnPingThread 1
    liftIO . putStrLn . ("Client version: " ++) =<< WS.getVersion
    sink <- WS.getSink
    (`catchWsError` handleErr sink) $ forever $ do
        msg <- WS.receiveData
        liftIO $ putStrLn ("Msg: " ++ show msg)
        case decode msg of
            Just Connect{..} -> do
                liftIO $ putStrLn ("Username: " ++ username)
                liftIO . atomically $ modifyTVar users (M.insert username sink)
                liftIO $ putStrLn (show (encode (Connected username)))
                liftIO $ WS.sendSink sink . DataMessage . Text . encode $
                    Connected username
                forever $ do
                    msg <- WS.receiveData
                    liftIO $ putStrLn ("Msg: " ++ show msg)
                    case decode msg of
                        Just Send{..} -> do
                            liftIO $ putStrLn ("Message: " ++ message)
                            users' <- liftIO . atomically $ readTVar users
                            liftIO $ forM_ (M.elems users') $ \ sink' -> do
                                (WS.sendSink sink' . DataMessage . Text . encode $
                                    Broadcast username message) `E.catch` \ (e :: SomeException) -> do
                                        putStrLn $ "got " ++ show e
                                        return ()
                        _ -> return ()
            _ -> return ()
  where
    handleErr sink e = liftIO $ do
        putStrLn $ "got " ++ show e
        atomically $ modifyTVar users (M.filter (/= sink))



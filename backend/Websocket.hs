{-# LANGUAGE Rank2Types, ScopedTypeVariables #-}
module Websocket where

import Control.Monad.IO.Class (liftIO,MonadIO)
import Data.Aeson hiding (json)
import Network.WebSockets hiding (receive,send,Sink)
import qualified Network.WebSockets as WS
import GHC.Exception

newtype Handler m a = Handler (a -> m (Handler m a))

type Sink = WS.Sink Hybi10

always :: Monad m => (a -> m ()) -> Handler m a
always k = Handler $ \ a -> do
    k a
    return (always k)

receive :: FromJSON a => WebSockets Hybi10 (Maybe a)
receive = fmap decode receiveData

send :: (ToJSON a,MonadIO m) => Sink -> a -> m ()
send s = liftIO . sendSink s . DataMessage . Text . encode

sendToMany :: (ToJSON a,MonadIO m) => [Sink] -> a -> m ()
sendToMany ss m = sequence_ [ send s m | s <- ss ]

handleRequest ::
    forall m a .
    (MonadIO m,FromJSON a) =>
    (forall t . m t -> IO t) ->
    -- ^ How to run the monad (reader monad, or the identity function)
    (Sink -> Handler m a) ->
    -- ^ Handle to create for a sink
    (Sink -> SomeException -> m ()) ->
    -- ^ Handle exceptions from this sink
    Request ->
    -- ^ Incoming request
    WebSockets Hybi10 ()
handleRequest run_m mk_h h_err rq = do
    acceptRequest rq
    spawnPingThread 1
    sink <- getSink
    loop (mk_h sink) `catchWsError` (m_to_ws . h_err sink)
  where
    m_to_ws :: forall t . m t -> WebSockets Hybi10 t
    m_to_ws = liftIO . run_m

    loop :: Handler m a -> WebSockets Hybi10 ()
    loop (Handler h) = do
        m_msg <- receive
        case m_msg of
            Just msg -> do
                h' <- m_to_ws (h msg)
                loop h'
            _ -> loop (Handler h)


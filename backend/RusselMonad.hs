{-# LANGUAGE GeneralizedNewtypeDeriving,NamedFieldPuns #-}
module RusselMonad where

import Control.Concurrent.STM

import Data.Aeson hiding (json)

import Control.Applicative hiding ((<|>))
import Control.Monad.Reader

import qualified Data.Map as M

import Data.Time.Clock

import Lexicon
import Websocket
import UserDB
import Grid

data RusselState
    = Play Grid UTCTime
    | Scoreboard UTCTime

data RusselEnv = RusselEnv
    { lexicon   :: Lexicon
    , trigrams  :: Trigrams
    , state     :: TVar RusselState
    , user_db   :: TVar UserDB
    , next_grid :: TMVar Grid
    }

newtype RusselM a = RusselM (ReaderT RusselEnv IO a)
  deriving (Monad,MonadReader RusselEnv,MonadIO,Functor,Applicative)

us :: Integer
us = 1000 * 1000

diffToMs :: NominalDiffTime -> Integer
diffToMs = truncate . (precision *) . toRational
  where precision = toRational us

runRusselM :: RusselEnv -> RusselM a -> IO a
runRusselM e (RusselM m) = runReaderT m e

play :: RusselState -> Bool
play Play{} = True
play _      = False

since :: RusselState -> UTCTime
since (Play _ t)     = t
since (Scoreboard t) = t


withGrid :: (Grid -> RusselM a) -> RusselM (Maybe a)
withGrid k = do
    RusselEnv{state} <- ask
    s <- liftIO $ atomically (readTVar state)
    case s of
        Play g _ -> Just <$> k g
        _        -> return Nothing

sendToAll :: ToJSON a => a -> RusselM ()
sendToAll m = do
    udb <- liftIO . atomically . readTVar =<< asks user_db
    sendToMany (map user_sink (M.elems udb)) m


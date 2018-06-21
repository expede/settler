{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

import           Control.Concurrent.STM (STM, TVar, atomically, modifyTVar,
                                         newTVarIO, readTVarIO)
import           Control.Monad          (forM_)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, ReaderT, ask, lift,
                                         runReaderT)


import qualified Data.Map.Strict        as Map
import           Data.Text.Lazy         (Text)

import           Network.HTTP.Types     (status404)

import           Web.Scotty.Trans

newtype SettlerM a = SettlerM { runSettlerM :: ReaderT (TVar Store) IO a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader (TVar Store)
           )

newtype Store = Store { getStore :: Map.Map Text Text }

type Handler m = ScottyT Text SettlerM m

main :: IO ()
main = do
  store <- newTVarIO $ Store Map.empty
  scottyT 4000 (flip runReaderT store . runSettlerM) $ do
    getter
    putter

getter :: Handler ()
getter =
  get "/get" $ do
    key   <- param "key"
    var   <- lift ask
    store <- liftIO $ readTVarIO var

    case Map.lookup key (getStore store) of
      Just value -> text value
      Nothing    -> status status404

putter :: Handler ()
putter =
  put "/set" $ do
    queries <- params
    var <- lift ask
    liftIO . atomically . forM_ queries $ update var

update :: TVar Store -> (Text, Text) -> STM ()
update var (key, value) =
  modifyTVar var (Store . Map.insert key value . getStore)

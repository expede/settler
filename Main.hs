{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}

import           Control.Concurrent.STM (STM, TVar, modifyTVar)
import           Control.Monad          (forM_)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, ask, lift)

import qualified Data.Map.Strict        as Map

import           RIO
import qualified RIO.Text.Lazy          as LTxt

import           Network.HTTP.Types     (status404)
import           Web.Scotty.Trans

newtype SettlerM a = SettlerM { runSettlerM :: RIO (TVar Store) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader (TVar Store)
           )

newtype Store = Store { getStore :: Map.Map LTxt.Text LTxt.Text }

type Controller m = ActionT LTxt.Text SettlerM m

main :: IO ()
main = do
  store <- newTVarIO $ Store Map.empty
  scottyT 4000 (runRIO store . runSettlerM) $ do
    get "/get" getter
    put "/set" putter

getter :: Controller ()
getter = do
  key   <- param "key"
  var   <- lift ask
  store <- liftIO $ readTVarIO var

  case Map.lookup key (getStore store) of
    Just value -> text value
    Nothing    -> status status404

putter :: Controller ()
putter = do
  queries <- params
  var     <- lift ask
  liftIO . atomically . forM_ queries $ update var

update :: TVar Store -> (LTxt.Text, LTxt.Text) -> STM ()
update var (key, value) = modifyTVar var (Store . Map.insert key value . getStore)

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}

import           RIO
import qualified RIO.Map            as Map
import qualified RIO.Text.Lazy      as LTxt

import           Network.HTTP.Types (status404)
import           Web.Scotty.Trans

newtype SettlerM a = SettlerM { runSettlerM :: RIO (TVar Store) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader (TVar Store)
           )

type Store = Map LTxt.Text LTxt.Text

type Controller m = ActionT LTxt.Text SettlerM m

main :: IO ()
main = do
  store <- newTVarIO Map.empty
  scottyT 4000 (runRIO store . runSettlerM) $ do
    get "/get" getter
    put "/set" putter

getter :: Controller ()
getter = do
  key   <- param "key"
  var   <- lift ask
  store <- liftIO $ readTVarIO var

  case Map.lookup key store of
    Just value -> text value
    Nothing    -> do
      status status404
      text $ "Unable to find key '" <> key <> "'"

putter :: Controller ()
putter = do
  queries <- params
  var     <- lift ask
  liftIO . atomically . forM_ queries $ update var

update :: TVar Store -> (LTxt.Text, LTxt.Text) -> STM ()
update var (key, value) = modifyTVar var $ Map.insert key value

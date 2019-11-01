{-# LANGUAGE OverloadedStrings #-}
module Example (runApp, app) where

import           Network.Wai (Application)
import           Web.Scotty
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as B8
import           Data.Maybe
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Database.Redis as Hedis
import           System.Environment
import           System.Random

-- Some useful functions
url_lifetime = 604800 -- 1 week in seconds

newRandomID :: IO B8.ByteString
newRandomID = B8.pack <$> replicateM 10 (randomRIO ('a','z'))

storeUrl :: Hedis.Connection -> B8.ByteString -> B8.ByteString -> ActionM (Either Hedis.Reply  Hedis.Status)
storeUrl connpool randomID longUrl = liftIO $ Hedis.runRedis connpool $ do
  Hedis.setex randomID url_lifetime longUrl

--the app itself
app' :: Hedis.Connection -> TL.Text -> ScottyM ()
app' connpool defaultUrl = do
  get "/healthcheck" $ text "I'm OK. Thanks for asking!"
  
  post "/new-url" $ do
    longURL <- (param "long_url")
    randomID <- liftIO newRandomID
    storeUrl connpool randomID (TE.encodeUtf8 longURL)
    text . TL.fromStrict . TE.decodeUtf8 $ randomID
  
  get "/:random_id" $ do
    randomID <- param "random_id"
    maybe_long_url <- liftIO $ Hedis.runRedis connpool (Hedis.get randomID)
    case maybe_long_url of
      Left _ -> redirect defaultUrl -- error returned by redis
      Right Nothing -> redirect defaultUrl -- redis call succeeded but the key was not present
      Right (Just long_url) -> redirect . TL.fromStrict . TE.decodeUtf8 $ long_url

-- Separate the app from running it, so that we can reuse the app for the test suite
runAppWith :: (ScottyM () -> IO b) -> IO b
runAppWith f = do
  connpool <- Hedis.checkedConnect Hedis.defaultConnectInfo
  defaultUrl <- TL.pack . fromMaybe "http://example.org" <$> lookupEnv "DEFAULT_URL"
  f (app' connpool defaultUrl)

-- for testing
app :: IO Application
app = runAppWith scottyApp

-- for running
runApp :: IO ()
runApp = runAppWith (scotty 8080)

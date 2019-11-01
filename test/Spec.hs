{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Main (main) where

import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON
import           Network.HTTP.Types.Header
import           Data.Aeson (Value(..), object, (.=))

import           Example (app)
import qualified Database.Redis as Hedis

main :: IO ()
main = do
  connpool <- Hedis.checkedConnect Hedis.defaultConnectInfo
  hspec $ before (flushRedis connpool) $ spec connpool

flushRedis :: Hedis.Connection -> IO ()
flushRedis connpool = do
  connpool <- Hedis.checkedConnect Hedis.defaultConnectInfo
  Hedis.runRedis connpool Hedis.flushall >> return ()

spec :: Hedis.Connection -> Spec
spec connpool = with app $ do
  describe "GET /healthcheck" $ do
    it "responds with 200" $ do
      get "/healthcheck" `shouldRespondWith` "I'm OK. thanks for asking!" { matchStatus = 200 }

  describe "GET /random_id" $ do
    it "redirects to the default url if the url is not present in redis" $ do
      get "/not-here" `shouldRespondWith` "" { matchStatus = 302
                                             , matchHeaders = ["Location" <:> "http://example.org"]}

    it "redirects to the correct url if the url is present in redis" $ do
      liftIO $ Hedis.runRedis connpool $ Hedis.set "correct-url" "http://not-example.org"
      get "/correct-url" `shouldRespondWith` "" { matchStatus = 302
                                                , matchHeaders = ["Location" <:> "http://not-example.org"]}

  describe "POST /new-url" $ do
    it "registers the url in Redis and it can be retrieved" $ do
      postHtmlForm "/new-url" [("long_url","http://example.org")] 
        `shouldRespondWith` 200
      Right numberOfKeys <- liftIO $ Hedis.runRedis connpool $ Hedis.dbsize
      liftIO $ numberOfKeys `shouldBe` ( 1 :: Integer )

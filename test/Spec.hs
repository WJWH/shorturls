{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Main (main) where

import           Test.Hspec
import           Test.Hspec.Wai
import           Network.HTTP.Types.Header
import           Network.Wai.Test

import           Example (app)
import qualified Database.Redis as Hedis
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL8

main :: IO ()
main = do
  connpool <- Hedis.checkedConnect Hedis.defaultConnectInfo
  hspec 
    $ before (flushRedis connpool) 
    $ afterAll_ (flushRedis connpool)
    $ spec connpool

flushRedis :: Hedis.Connection -> IO ()
flushRedis connpool = do
  connpool <- Hedis.checkedConnect Hedis.defaultConnectInfo
  Hedis.runRedis connpool Hedis.flushall >> return ()

spec :: Hedis.Connection -> Spec
spec connpool = with app $ do
  describe "GET /healthcheck" $ do
    it "responds with 200" $ do
      get "/healthcheck" `shouldRespondWith` "I'm OK. Thanks for asking!" { matchStatus = 200 }

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

  describe "entire flow" $ do
    it "redirects to the URL you just inserted" $ do
      postResponse <- postHtmlForm "/new-url" [("long_url","http://awesome-website.org")]
      let randomID = BL8.toStrict . BL8.reverse . BL8.take 10 . BL8.reverse . simpleBody $ postResponse
      get ("/" <> randomID) `shouldRespondWith` "" { matchStatus = 302
                                                   , matchHeaders = ["Location" <:> "http://awesome-website.org"]}
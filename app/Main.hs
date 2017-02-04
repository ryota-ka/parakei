{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Conduit (newManager, tlsManagerSettings)
import System.Environment (getArgs, getEnv)
import System.Random (randomRIO)
import Web.Authenticate.OAuth (def, newCredential, oauthConsumerKey, oauthConsumerSecret)
import Web.Twitter.Conduit (call, setCredential, TWInfo, TwitterError(TwitterErrorResponse), TwitterErrorMessage(..), twitterOAuth, update)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T

getTwInfoFromEnv :: IO TWInfo
getTwInfoFromEnv = do
    let keys = [ "PARAKEI_CONSUMER_KEY"
               , "PARAKEI_CONSUMER_SECRET"
               , "PARAKEI_ACCESS_TOKEN"
               , "PARAKEI_ACCESS_TOKEN_SECRET"
               ]
    [consumerKey, consumerSecret, accessToken, accessTokenSecret] <- mapM envBS keys
    let tokens = buildOAuth consumerKey consumerSecret
        credential = newCredential accessToken accessTokenSecret
    pure $ setCredential tokens credential def
    where
      envBS key = BS.pack <$> getEnv key
      buildOAuth key secret = twitterOAuth { oauthConsumerKey = key, oauthConsumerSecret = secret }

tweet :: T.Text -> IO ()
tweet text = do
    manager <- newManager tlsManagerSettings
    twInfo <- getTwInfoFromEnv
    _ <- call twInfo manager (update text)
    pure ()

main :: IO ()
main = do
  filename <- head <$> getArgs
  words <- T.lines . T.pack <$> readFile filename
  i <- randomRIO (0, length words - 1)
  tweet "今から経済学部っぽいこと言いま〜〜〜〜〜すｗｗｗｗｗｗｗｗｗｗｗｗｗｗｗ ｳｪｲｗｗｗｗｗｗｗｗｗｗｗｗｗｗｗｗｗｗｗｗｗｗｗｗｗｗｗｗｗ"
  threadDelay 5000000
  tweet (words !! i)

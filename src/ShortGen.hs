{-# LANGUAGE OverloadedStrings #-}

module ShortGen where

import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BC
import Data.Text.Encoding
  (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Database.Redis as R
import Network.URI (URI, parseURI)
import qualified System.Random as SR
import Web.Scotty

alphaNum :: String
alphaNum = ['A'..'Z'] ++ ['0'..'9']

randomElement :: String -> IO Char
randomElement xs = do
  let maxIndex :: Int
      maxIndex = length xs - 1
  randomDigit <- SR.randomRIO (0, maxIndex)
  return $ xs !! randomDigit

shortyGen :: IO [Char]
shortyGen =
  replicateM 7 $ randomElement alphaNum

-- -- use to test key collisions
-- shortyGen :: IO [Char]
-- shortyGen = return "DEFAULTKEY"


saveURI :: R.Connection
        -> BC.ByteString
        -> BC.ByteString
        -> IO (Either R.Reply R.Status)
saveURI conn shortURI uri =
  R.runRedis conn $ R.set shortURI uri

getURI :: R.Connection
       -> BC.ByteString
       -> IO (Either R.Reply
              (Maybe BC.ByteString))
getURI conn shortURI =
  R.runRedis conn $ R.get shortURI

linkShorty :: String -> String
linkShorty shorty =
  concat
  [ "<a href=\""
  , shorty
  , "\">Copy and paste your short URL</a>"
  ]

-- TL.concat :: [TL.Text] -> TL.Text
shortyCreated :: Show a
              => a
              -> String
              -> TL.Text

shortyCreated resp short =
  TL.concat [ TL.pack (show resp)
            , " short URL is: "
            , TL.pack (linkShorty short)
            ]

shortyIsNotUri :: TL.Text -> TL.Text
shortyIsNotUri uri =
  TL.concat
    [ uri
    , " wasn't a url,"
    , " did you forget http://?"
    ]

shortyFound :: TL.Text -> TL.Text
shortyFound tbs =
  TL.concat
    [ "<a href=\""
    , tbs, "\">"
    , tbs, "</a>" ]


app :: R.Connection
    -> ScottyM ()
app rConn = do
  get "/" $ do
    uri <- param "uri"
    let parsedUri :: Maybe URI
        parsedUri =
          parseURI (TL.unpack uri)

    case parsedUri of
      Just _ -> do
        sg <- liftIO shortyGen
        let shorty = BC.pack sg
            uri' =
              encodeUtf8 (TL.toStrict uri)
        existingURI <-
          liftIO (getURI rConn shorty)

        case existingURI of
          Left reply ->
            text (TL.pack $ show reply)
          Right mbBS -> case mbBS of
            Just _ -> text "short URI is\
              \ already in use. Please\
              \ retry the query."
            Nothing ->
              liftIO
                (saveURI rConn shorty uri') >>=
                (\resp ->
                  html (shortyCreated resp sg))

      Nothing -> text (shortyIsNotUri uri)

  get "/:short" $ do
    short <- param "short"
    uri <- liftIO (getURI rConn short)

    case uri of
      Left reply ->
        text (TL.pack $ show reply)
      Right mbBS -> case mbBS of
        Nothing -> text "uri not found"
        Just bs -> html (shortyFound tbs)

          where tbs :: TL.Text
                tbs =
                  TL.fromStrict
                    (decodeUtf8 bs)


main :: IO ()
main = do
  rConn <- R.connect R.defaultConnectInfo
  scotty 3000 $ app rConn

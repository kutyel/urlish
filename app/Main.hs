{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad          (replicateM)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8  as BC
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy         as TL
import qualified Database.Redis         as R
import           Network.URI            (URI, parseURI)
import           System.Random          (randomRIO)
import           Web.Scotty

alphaNum :: String
alphaNum = ['a' .. 'z'] ++ ['0' .. '9']

randomElement :: String -> IO Char
randomElement xs = randomRIO (0, length xs - 1) >>= \rand -> return (xs !! rand)

shortyGen :: IO String
shortyGen = replicateM 7 (randomElement alphaNum)

saveURI ::
     R.Connection
  -> BC.ByteString
  -> BC.ByteString
  -> IO (Either R.Reply R.Status)
saveURI conn shortURI uri = R.runRedis conn $ R.set shortURI uri

getURI ::
     R.Connection -> BC.ByteString -> IO (Either R.Reply (Maybe BC.ByteString))
getURI conn shortURI = R.runRedis conn $ R.get shortURI

linkShorty :: String -> String
linkShorty shorty =
  concat ["<a href=\"", shorty, "\">Copy and paste your short URL!</a>"]

shortyCreated :: Show a => a -> String -> TL.Text
shortyCreated resp urlish =
  TL.concat [TL.pack (show resp), " shorty is: ", TL.pack (linkShorty urlish)]

shortyAintUri :: TL.Text -> TL.Text
shortyAintUri uri = TL.concat [uri, " wasn't a url, did you forget http://?"]

shortyFound :: TL.Text -> TL.Text
shortyFound tbs = TL.concat ["<a href=\"", tbs, "\">", tbs, "</a>"]

app :: R.Connection -> ScottyM ()
app rConn = do
  get "/" $ do
    uri <- param "uri"
    let parsedUri :: Maybe URI
        parsedUri = parseURI (TL.unpack uri)
    case parsedUri of
      Just _ -> do
        urlish <- liftIO shortyGen
        let shorty = BC.pack urlish
            uri' = encodeUtf8 (TL.toStrict uri)
        urz <- liftIO (getURI rConn shorty)
        case urz of
          Left reply -> text (TL.pack (show reply))
          Right mbBS ->
            case mbBS of
              Nothing -> do
                resp <- liftIO (saveURI rConn shorty uri')
                html (shortyCreated resp urlish)
              Just _ -> text "hash already in use!"
      Nothing -> text (shortyAintUri uri)
  get "/:short" $ do
    short <- param "short"
    uri <- liftIO (getURI rConn short)
    case uri of
      Left reply -> text (TL.pack (show reply))
      Right mbBS ->
        case mbBS of
          Nothing -> text "uri not found"
          Just bs -> html (shortyFound tbs)
            where tbs :: TL.Text
                  tbs = TL.fromStrict (decodeUtf8 bs)

main :: IO ()
main = R.connect R.defaultConnectInfo >>= \conn -> scotty 3000 (app conn)

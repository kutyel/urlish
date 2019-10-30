{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad              (replicateM)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans        (lift)
import           Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import qualified Data.ByteString.Char8      as BC
import           Data.Text.Encoding         (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy             as TL
import qualified Database.Redis             as R
import           Network.URI                (URI, parseURI)
import           System.Random              (randomRIO)
import           Web.Scotty.Trans

type ActionR = ActionT TL.Text (ReaderT R.Connection IO)

alphaNum :: String
alphaNum = ['a' .. 'z'] ++ ['0' .. '9']

randomElement :: String -> IO Char
randomElement xs = randomRIO (0, length xs - 1) >>= \rand -> return (xs !! rand)

shortyGen :: IO String
shortyGen = replicateM 7 (randomElement alphaNum)

runDb :: R.Redis (Either R.Reply b) -> ActionR (Either R.Reply b)
runDb action = lift ask >>= \conn -> liftIO $ R.runRedis conn action

saveURI :: BC.ByteString -> BC.ByteString -> ActionR (Either R.Reply R.Status)
saveURI shortURI uri = runDb $ R.set shortURI uri

getURI :: BC.ByteString -> ActionR (Either R.Reply (Maybe BC.ByteString))
getURI shortURI = runDb $ R.get shortURI

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

app :: ScottyT TL.Text (ReaderT R.Connection IO) ()
app = do
  get "/" $ do
    uri <- param "uri"
    let parsedUri :: Maybe URI
        parsedUri = parseURI (TL.unpack uri)
    case parsedUri of
      Nothing -> text (shortyAintUri uri)
      Just _ -> do
        urlish <- liftIO shortyGen
        let shorty = BC.pack urlish
            uri' = encodeUtf8 (TL.toStrict uri)
        urz <- getURI shorty
        case urz of
          Left reply -> text (TL.pack (show reply))
          Right mbBS ->
            case mbBS of
              Nothing -> do
                resp <- saveURI shorty uri'
                html (shortyCreated resp urlish)
              Just _ -> text "hash already in use!"
  get "/:short" $ do
    short <- param "short"
    uri <- getURI short
    case uri of
      Left reply -> text (TL.pack (show reply))
      Right mbBS ->
        case mbBS of
          Nothing -> text "uri not found"
          Just bs -> html (shortyFound tbs)
            where tbs :: TL.Text
                  tbs = TL.fromStrict (decodeUtf8 bs)

main :: IO ()
main =
  flip (scottyT 3000 . flip runReaderT) app =<< R.connect R.defaultConnectInfo

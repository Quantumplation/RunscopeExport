{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import           BasePrelude                (Either, IO, Int, String,
                                             appendFile, filter, fmap, foldM,
                                             foldM_, head, length, map, mapM,
                                             mapM_, otherwise, putStrLn, return,
                                             show, take, undefined, ($), (+),
                                             (++), (.), (=<<))
import           Control.Concurrent         (forkIO)
import           Control.Concurrent.Chan
import           Control.Lens
import qualified Control.Monad.Parallel     as MP
import           Data.Aeson
import           Data.Aeson.Lens            (key, values, _String, _Value)
import           Data.Aeson.Types           hiding (Options)
import           Data.ByteString.Char8      (ByteString, concat, lines, pack,
                                             readFile, unpack)
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Functor               ((<$), (<$>))
import           Data.Map                   (Map, alter, empty, insert, lookup)
import           Data.Maybe
import qualified Data.Text                  as T
import           Data.Text.Encoding         (decodeUtf8, encodeUtf8)
import           Debug.Trace
import           Network.Wreq
import qualified Network.Wreq.Session       as S

-- Secrets
authToken :: IO ByteString
authToken = head . lines <$> readFile "auth_token.secret"

bucketKeys :: IO [ByteString]
bucketKeys = lines <$> readFile "bucket_key.secret"

-- Constants
rootUrl :: ByteString
rootUrl = "https://api.runscope.com/buckets/"

folder :: String
folder = "requests/"

maxRequests :: Int
maxRequests = 1000

-- Uri builders
captureUrl :: ByteString -> ByteString
captureUrl bucket = concat [rootUrl, bucket, "/captures"]

messageUrl :: ByteString -> ByteString -> ByteString
messageUrl bucket uuid = concat [rootUrl, bucket, "/messages/", uuid]

filename :: ByteString -> Int -> String
filename eventType count = folder ++ unpack eventType ++ show count ++ ".json"

-- Helpers
withToken :: ByteString -> Options -> Options
withToken t o = o & header "Authorization" .~ [concat ["Bearer ", t]]

getAuth :: Options -> S.Session -> ByteString -> IO (Response BL.ByteString)
getAuth opts sess url = do
    token <- authToken
    S.getWith (withToken token opts) sess (unpack url)

spawn :: (Chan a -> IO ()) -> IO (Chan a)
spawn func = do
    c <- newChan
    forkIO $ func c
    return c
    
-- Lenses and data conversion
getData r = fromMaybe emptyArray $ r ^? responseBody . key "data" . _Value
getBody r = fromMaybe "" $ getData r ^? key "request" . key "body" . _String
getEventType v = fromMaybe "request" $ v ^? key "eventType" . _String

parseUUID encoded = case fromJSON encoded of
    Error err -> Nothing
    Success a -> Just $ pack a

-- Web-requests and parsing
getUUIDs :: S.Session -> ByteString -> IO [ByteString]
getUUIDs sess bucket = do
    let opts = defaults & param "count" .~ [(T.pack . show) maxRequests]
    let url = captureUrl bucket
    response <- getAuth opts sess url
    let responseData = getData response
    let uuids = responseData ^.. values . key "uuid"
    return $ mapMaybe parseUUID uuids

getRequestBody :: S.Session -> ByteString -> ByteString -> IO Value
getRequestBody sess bucket uuid = ourDecode <$> getAuth defaults sess url
    where
        url = messageUrl bucket uuid
        ourDecode = fromMaybe emptyObject . decode . BL.fromStrict . encodeUtf8 . getBody

-- Handling API responses
handleCategory :: ByteString -> Int -> Chan Value -> IO ()
handleCategory category count chan = do
    nextVal <- readChan chan
    putStrLn $ "Writing " ++ file
    BL.writeFile file $ encode nextVal
    handleCategory category (count + 1) chan
    where
        file = filename category count

handleResponse :: Map ByteString (Chan Value) -> Chan Value -> IO ()
handleResponse categoryTable recvChan = do
    nextResponse <- readChan recvChan
    let eventType = encodeUtf8 $ getEventType nextResponse
    (chan, newTable) <- case lookup eventType categoryTable of
                Nothing -> do
                    c <- spawn $ handleCategory eventType 1
                    return (c, insert eventType c categoryTable)
                Just c  -> return (c, categoryTable)
    writeChan chan nextResponse
    handleResponse newTable recvChan

handleUUID :: S.Session -> ByteString -> Chan Value -> ByteString -> IO ()
handleUUID sess bucket responseChan uuid =
    writeChan responseChan =<< getRequestBody sess bucket uuid

handleBucket :: S.Session -> ByteString -> IO ()
handleBucket sess bucket = do
    uuids <- getUUIDs sess bucket
    putStrLn $ "Processing " ++ show (length uuids) ++ " uuids."
    responseChan <- spawn $ handleResponse empty
    _ <- MP.mapM (handleUUID sess bucket responseChan) uuids
    return ()

-- Entrypoint
main :: IO ()
main = S.withAPISession $ \sess -> do
    putStrLn "Start..."
    mapM_ (handleBucket sess) =<< bucketKeys
    putStrLn "Done"

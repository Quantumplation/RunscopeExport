{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           BasePrelude                (IO, Int, String, appendFile,
                                             filter, fmap, foldM, foldM_, head,
                                             length, map, mapM, mapM_,
                                             otherwise, putStrLn, return, show,
                                             take, undefined, writeFile, ($),
                                             (+), (++), (.), (=<<))
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens            (key, values, _String, _Value)
import           Data.Aeson.Types           hiding (Options)
import           Data.ByteString.Char8      (ByteString, concat, lines, pack,
                                             readFile, unpack)
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Map                   (Map, alter, empty, lookup)
import           Data.Maybe
import qualified Data.Text                  as T
import           Data.Text.Encoding         (encodeUtf8)
import           Debug.Trace
import           Network.Wreq

-- Secrets
authToken :: IO ByteString
authToken = readFile "auth_token.secret"

bucketKeys :: IO [ByteString]
bucketKeys = do
    contents <- readFile "bucket_key.secret"
    return $ lines contents

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

-- Request helpers
withToken :: ByteString -> Options -> Options
withToken t o = o & header "Authorization" .~ [concat ["Bearer ", t]]

getAuth :: Options -> ByteString -> IO (Response BL.ByteString)
getAuth opts url = do
    token <- authToken
    getWith (withToken token opts) (unpack url)

-- Lenses and data conversion
getData r = fromMaybe emptyArray $ r ^? responseBody . key "data" . _Value
getBody r = fromMaybe "" $ getData r ^? key "request" . key "body" . _String
getEventType v = fromMaybe "request" $ v ^? key "eventType" . _String

parseUUID encoded = case fromJSON encoded of
    Error err -> Nothing
    Success a -> Just $ pack a

-- Web-requests and parsing
getUUIDs :: ByteString -> IO Value
getUUIDs bucket = do
    let opts = defaults & param "count" .~ [(T.pack . show) maxRequests]
    let url = captureUrl bucket
    response <- getAuth opts url
    let responseData = getData response
    let uuids = responseData ^.. values . key "uuid"
    return $ mapMaybe parseUUID uuids

getRequestBody :: ByteString -> ByteString -> IO Value
getRequestBody bucket uuid = do
    let url = messageUrl bucket uuid
    response <- getAuth defaults url
    let bodyAsString = getBody response
    let attemptedDecode = (decode . BL.fromStrict . encodeUtf8) details :: Maybe Value
    return fromMaybe emptyObject maybeDecoded

-- Handling API responses
handleResponse :: Map ByteString Int -> Value -> IO (Map ByteString Int)
handleResponse counts v = do
    let eventType = encodeUtf8 $ getEventType v
    let count = modify $ lookup eventType counts
    let file = filename eventType count
    putStrLn $ "Writing " ++ file
    writeFile file $ (BL.unpack . encode) v
    let newCounts = alter (Just . modify) eventType counts
    return newCounts
    where
        modify a = fromMaybe 0 a + 1

handleUUID :: ByteString -> Map ByteString Int -> ByteString -> IO (Map ByteString Int)
handleUUID bucket counts uuid = do
    details <- getRequestBody bucket uuid
    handleResponse counts details


handleBucket :: ByteString -> IO ()
handleBucket bucket = do
    uuids <- getUUIDs bucket
    putStrLn $ "Processing " ++ show (length uuids) ++ " uuids."
    foldM_ (handleUUID bucket) empty uuids

-- Entrypoint
main :: IO ()
main = do
    putStrLn "Start..."
    buckets <- bucketKeys
    mapM_ handleBucket buckets
    putStrLn "Done"

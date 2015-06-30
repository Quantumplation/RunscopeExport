{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import           BasePrelude                (IO, Int, String, appendFile,
                                             filter, fmap, foldM, foldM_, head,
                                             length, map, mapM, mapM_,
                                             otherwise, putStrLn, return, show,
                                             take, undefined, ($), (+), (++),
                                             (.), (=<<))
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens            (key, values, _String, _Value)
import           Data.Aeson.Types           hiding (Options)
import           Data.ByteString.Char8      (ByteString, concat, lines, pack,
                                             readFile, unpack)
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Functor               ((<$), (<$>))
import           Data.Map                   (Map, alter, empty, lookup)
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

-- Request helpers
withToken :: ByteString -> Options -> Options
withToken t o = o & header "Authorization" .~ [concat ["Bearer ", t]]

getAuth :: Options -> S.Session -> ByteString -> IO (Response BL.ByteString)
getAuth opts sess url = do
    token <- authToken
    S.getWith (withToken token opts) sess (unpack url)

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
handleResponse :: Map ByteString Int -> Value -> IO (Map ByteString Int)
handleResponse counts v = newCounts <$ do
    putStrLn $ "Writing " ++ file
    BL.writeFile file $ encode v
    where
        eventType = encodeUtf8 $ getEventType v
        count = modify $ lookup eventType counts
        file = filename eventType count
        newCounts = alter (Just . modify) eventType counts
        modify = maybe 1 (+1)

handleUUID :: S.Session -> ByteString -> Map ByteString Int -> ByteString -> IO (Map ByteString Int)
handleUUID sess bucket counts uuid = handleResponse counts =<< getRequestBody sess bucket uuid

handleBucket :: S.Session -> ByteString -> IO ()
handleBucket sess bucket = do
    uuids <- getUUIDs sess bucket
    putStrLn $ "Processing " ++ show (length uuids) ++ " uuids."
    foldM_ (handleUUID sess bucket) empty uuids

-- Entrypoint
main :: IO ()
main = S.withAPISession $ \sess -> do
    putStrLn "Start..."
    mapM_ (handleBucket sess) =<< bucketKeys
    putStrLn "Done"

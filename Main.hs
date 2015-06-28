{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           BasePrelude                (IO, Int, String, appendFile,
                                             filter, fmap, foldM, foldM_, head,
                                             length, map, mapM, otherwise,
                                             putStrLn, return, show, take,
                                             undefined, writeFile, ($), (+),
                                             (++), (.), (=<<))
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


authToken :: IO ByteString
authToken = readFile "auth_token.secret"

bucketKey :: IO ByteString
bucketKey = do
    contents <- readFile "bucket_key.secret"
    return $ (head . lines) contents

rootUrl :: ByteString
rootUrl = "https://api.runscope.com/buckets/"

captureUrl :: ByteString -> ByteString
captureUrl bucketKey = concat [rootUrl, bucketKey, "/captures"]

messageUrl :: ByteString -> ByteString -> ByteString
messageUrl bucketKey uuid = concat [rootUrl, bucketKey, "/messages/", uuid]

withToken :: ByteString -> Options -> Options
withToken t o = o & header "Authorization" .~ [concat ["Bearer ", t]]

getAuth :: Options -> ByteString -> IO (Response BL.ByteString)
getAuth opts url = do
    token <- authToken
    getWith (withToken token opts) (unpack url)

getData r = fromMaybe emptyArray $ r ^? responseBody . key "data" . _Value
getBody r = fromMaybe "" $ getData r ^? key "request" . key "body" . _String

getDetails :: ByteString -> IO T.Text
getDetails uuid = do
    bKey <- bucketKey
    let url = messageUrl bKey uuid
    response <- getAuth defaults url
    return $ getBody response

folder :: String
folder = "requests/"

handleResponse :: Map ByteString Int -> T.Text -> IO (Map ByteString Int)
handleResponse counts v = do
    let v' = fromMaybe emptyObject ((decode $ BL.fromStrict $ encodeUtf8 v) :: Maybe Value)
    let reqType = encodeUtf8 $ fromMaybe "request" $ v' ^? key "eventType" . _String
    let count = modify $ lookup reqType counts
    let file = folder ++ unpack reqType ++ show count ++ ".json"
    putStrLn $ "Writing " ++ file
    writeFile file $ (BL.unpack . encode) v'
    let newCounts = alter (Just . modify) reqType counts
    return newCounts
    where
        modify a = fromMaybe 0 a + 1

handleUUID :: Map ByteString Int -> ByteString -> IO (Map ByteString Int)
handleUUID counts uuid = do
    deets <- getDetails uuid
    handleResponse counts deets

parseUUID encoded =
    let json = fromJSON encoded
    in case json of
        Error err -> Nothing
        Success a -> Just $ pack a

main = do
    putStrLn "Start..."
    url <- fmap captureUrl bucketKey
    let opts = defaults & param "count" .~ ["1000"]
    response <- getAuth opts url
    let body = getData response
    let uuids = body ^.. values . key "uuid"
    let extracted = mapMaybe parseUUID uuids
    putStrLn $ "Processing " ++ show (length extracted) ++ " uuids."
    foldM_ handleUUID empty extracted
    putStrLn "Done"

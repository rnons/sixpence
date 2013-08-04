{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Sixpence.Lyric where

import Codec.Binary.UTF8.String (decodeString)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LC8
import Data.Conduit (($$+-))
import Data.Conduit.Attoparsec (sinkParser)
import Data.List (intercalate)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.HTTP.Conduit
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor



data GeciMe = GeciMe
    { count :: Int
    , code  :: Int
    , result :: [Geci]
    } deriving (Show, Generic)

data Geci = Geci
    { aid :: Int
    , song :: String
    , artist :: String
    , lrc :: String
    , sid :: Int
    } deriving (Show, Generic)

instance FromJSON GeciMe
instance FromJSON Geci

wikia :: String -> String -> IO String
wikia artist song = do
    let url = "http://lyrics.wikia.com/api.php" ++
              "?artist=" ++ artist ++
              "&song=" ++ song ++
              "&fmt=xml"

    rsp <- simpleHttp url
    let cur = fromDocument $ parseLBS rsp
        lyrics = head $ cur $// element "lyrics" &// content
        lrcurl = cur $// element "url" &// content

    if lyrics == ("Not found" :: T.Text)
        then geci artist song
        else wikiaLyric $ T.unpack $ head lrcurl

wikiaLyric url = do
    print url
    rsp <- simpleHttp url
    let cur = fromDocument $ parseLBS rsp
    let lrc = cur $// element "div" 
                  >=> attributeIs "class" "lyricbox" 
                  &// content
    return $ intercalate "<br/>" $ init $ init $ tail $ map T.unpack lrc

geci :: String -> String -> IO String
geci artist song = do
    let url = "http://geci.me/api/lyric/" ++ song ++ "/" ++ artist
    req <- parseUrl url
    val <- withManager $ \manager -> do
        res <- http req manager
        responseBody res $$+- sinkParser json
    case fromJSON val :: Result GeciMe of
        Success s -> if count s > 0 then geciLyric $ lrc $ head $ result s
                                    else return "Not found"
        Error err -> putStrLn err >> print val >> return "Not found"

geciLyric url = do
    print url
    rsp <- simpleHttp url
    return $ intercalate "<br/>" 
           $ map (dropWhile (< 'z')) 
           $ lines $ decodeString $ LC8.unpack rsp

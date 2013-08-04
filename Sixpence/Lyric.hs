{-# LANGUAGE OverloadedStrings #-}

module Sixpence.Lyric where

import Data.List (intercalate)
import qualified Data.Text as T
import Network.HTTP.Conduit
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor

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
        then return $ T.unpack lyrics
        else wikiaLyric $ T.unpack $ head lrcurl

wikiaLyric url = do
    print url
    rsp <- simpleHttp url
    let cur = fromDocument $ parseLBS rsp
    let lrc = cur $// element "div" 
                  >=> attributeIs "class" "lyricbox" 
                  &// content
    return $ intercalate "<br/>" $ init $ init $ tail $ map T.unpack lrc


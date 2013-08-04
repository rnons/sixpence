{-# LANGUAGE ScopedTypeVariables #-}

import Codec.Binary.UTF8.String (decodeString)
import Control.Concurrent
import qualified Control.Exception as E
import Control.Monad
import qualified Data.ByteString.Char8 as C8
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, fromJust)
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Network.MPD
import qualified Network.MPD as MPD
import Reactive.Banana
import Reactive.Banana.Threepenny

import Sixpence.Lyric

main :: IO ()
main =
    startGUI Config
        { tpPort        = 10005
        , tpCustomHTML  = Nothing
        , tpStatic      = "static"
        } setup

setup :: Window -> IO ()
setup w = do
    return w # set title "A barebone MPD client"

    btPause     <- UI.button
    btNext      <- UI.button # set UI.text "Next"
    btLyric     <- UI.button # set UI.text "Fetch Lyric"
    eleInfo     <- UI.span
    elePlaying  <- UI.span
    eleShowLrc  <- UI.div
    element eleInfo # set text "Current Song: "
    
    getBody w #+
        [ row [ element eleInfo, element elePlaying ]
        , row [ element btPause, element btNext, element btLyric ]
        --, row [ element btPause, element btNext ]
        , row [ element eleShowLrc ]
        ]
    (mpdHandler, mpdSink) <- newAddHandler
    let 
        networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = do
            ePause <- event UI.click btPause
            eNext  <- event UI.click btNext
            eLyric <- event UI.click btLyric
            bMpd <- fromChanges ("", "") mpdHandler

            let 
                mPause :: Frameworks s => Moment s ()
                mPause = do
                    st <- liftIO $ withMPD status
                    if fmap stState st == Right Playing
                        then do
                            liftIO $ withMPD $ MPD.pause True 
                            void $ liftIO $ element btPause # set text "Play"
                        else do
                            liftIO $ withMPD $ MPD.pause False 
                            void $ liftIO $ element btPause # set text "Pause"
                
                mNext :: Frameworks s => Moment s ()
                mNext = void $ liftIO $ withMPD next

                mLyric :: Frameworks s => Moment s ()
                mLyric = liftIO $ do
                    artist <- mpdMeta Artist
                    title <- mpdMeta Title
                    lyric <- wikia artist title
                    void $ liftIO $ element eleShowLrc # set html lyric

            execute $ FrameworksMoment mPause <$ ePause
            execute $ FrameworksMoment mNext  <$ eNext
            execute $ FrameworksMoment mLyric <$ eLyric
            return btPause # sink text (fst <$> bMpd)
            return elePlaying # sink text (snd <$> bMpd)
        loop = do
            element eleShowLrc # set html ""
            st <- withMPD status
            let state = if fmap stState st == Right Playing 
                           then "Pause" else "Play"
            song <- mpdPlaying
            mpdSink (state, song)
            withMPD $ idle [PlayerS]
            loop
    network <- compile networkDescription
    actuate network
    void $ forkIO loop

mpdPlaying :: IO String
mpdPlaying = do
    title <- mpdMeta Title
    artist <- mpdMeta Artist
    return (artist ++ " - " ++ title)

mpdMeta :: Metadata -> IO String
mpdMeta info = E.catch
    (do song <- withMPD currentSong
        let metaValue = liftM (Map.lookup info . sgTags . fromJust) song
            meta = (\(Value v) -> v) $ head $ fromJust $ (\(Right v) -> v) metaValue
        return $ decodeString $ C8.unpack meta)
    (\e -> do print (e :: E.SomeException)
              return "")

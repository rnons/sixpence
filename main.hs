{-# LANGUAGE ScopedTypeVariables #-}

import Codec.Binary.UTF8.String (decodeString)
import Control.Concurrent
import Control.Monad
import qualified Data.ByteString.Char8 as C8
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Network.MPD
import qualified Network.MPD as MPD
import Reactive.Banana
import Reactive.Banana.Threepenny

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

    elePause <- UI.button
    eleNext <- UI.button # set UI.text "Next"
    eleInfo <- UI.span
    elePlaying <- UI.span
    element eleInfo # set text "Current Song: "
    
    getBody w #+
        [ row [ element eleInfo, element elePlaying ]
        , row [ element elePause, element eleNext]
        ]
    (mpdHandler, mpdSink) <- newAddHandler
    let 
        networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = do
            ePause <- event UI.click elePause
            eNext  <- event UI.click eleNext
            bMpd <- fromChanges ("", "") mpdHandler

            let 
                mPause :: Frameworks s => Moment s ()
                mPause = do
                    st <- liftIO $ withMPD status
                    if fmap stState st == Right Playing
                        then do
                            liftIO $ withMPD $ MPD.pause True 
                            void $ liftIO $ element elePause # set text "Play"
                        else do
                            liftIO $ withMPD $ MPD.pause False 
                            void $ liftIO $ element elePause # set text "Pause"
                mNext :: Frameworks s => Moment s ()
                mNext = void $ liftIO $ withMPD next

            execute $ FrameworksMoment mPause <$ ePause
            execute $ FrameworksMoment mNext <$ eNext
            return elePause # sink text (fst <$> bMpd)
            return elePlaying # sink text (snd <$> bMpd)
        loop = do
            st <- withMPD status
            let state = if fmap stState st == Right Playing 
                           then "Pause" else "Play"
            song <- mpdPlaying
            mpdSink (state, song)
            withMPD $ idle [PlayerS]
            loop
    network <- compile networkDescription
    actuate network
    void $ forkIO $ loop

mpdPlaying :: IO String
mpdPlaying = do
    title <- mpdMeta Title
    artist <- mpdMeta Artist
    return (artist ++ " - " ++ title)

mpdMeta :: Metadata -> IO String
mpdMeta info = do
    song <- withMPD currentSong
    let metaValue = liftM (Map.lookup info . sgTags . fromJust) song
        meta = (\(Value v) -> v) $ head $ fromJust $ (\(Right v) -> v) metaValue
    return $ decodeString $ C8.unpack meta

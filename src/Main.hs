{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.IO.Class
import Data.IORef
import qualified Data.IntMap as M
import Data.Unique
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Control.Monad

main :: IO ()
main = startGUI defaultConfig {jsPort = Just 8023} setup

data EHandler a = EHandler
  { listen :: (a -> UI ()) -> UI ()
  , fire :: a -> UI ()
  }

data EType
  = RadioStart
  | RadioStop
  | RecordingStart
  | RecordingStop
  | ChangeMessage String
  | VolUp
  | VolDown
  | ChangeChannel Channel
  | GetChannel

type Channel = (String, Float)

newEHandler :: UI (EHandler a)
newEHandler = do
  handlers <- liftIO $ newIORef M.empty
  let listen' fn = do
        key <- liftIO $ fmap hashUnique newUnique
        liftIO . modifyIORef handlers $ M.insert key fn
      fire' a = do
        hs <- liftIO $ readIORef handlers
        mapM_ ($ a) . M.elems $ hs
  return $ EHandler listen' fire'

createSelectBox opts = UI.select #+ map f opts
  where
    f (t, v) = UI.option # set UI.value v # set UI.text t

channels = [("OE3", 99.90), ("Kronehit", 105.80), ("FM4", 103.80), ("NRJ", 104.20), ("Mein Kinderradio", 103.20)]
channelsFS = map (\(a, b) -> (a, show b)) channels

setup :: Window -> UI ()
setup window
 = do

  -- window properties
  pure window # set UI.title "Radio Tuner"

  -- state
  radioS <- liftIO $ newIORef False
  recordingS <- liftIO $ newIORef False
  messageLogS <- liftIO $ newIORef ("" :: String)
  volS <- liftIO $ newIORef (7 :: Int)
  selChannelS <- liftIO $ newIORef (head channels :: Channel)

  -- simple event handler
  (e :: EHandler EType) <- newEHandler

  -- elements
  radioBtn <- UI.button # set UI.text "Start Radio"
  recordingBtn <- UI.button # set UI.text "Start Recording"
  messageLog <- UI.pre # set UI.text "Log is displayed here"
                       # set UI.style [("max-height", "200px"), ("overflow", "auto")]
  volDis <- UI.span # set UI.text "7"
  volPlus <- UI.button # set UI.text "[ + ]"
  volMinus <- UI.button # set UI.text "[ - ]"
  selectBox <- createSelectBox channelsFS

  -- main event handler function
  let defHandler RadioStart = do
        fire e (ChangeMessage "Radio Started")
        liftIO . modifyIORef radioS $ const True
        element radioBtn # set UI.text "Stop Radio"
        return ()
      defHandler RadioStop = do
        fire e RecordingStop
        fire e (ChangeMessage "Radio Stopped")
        liftIO . modifyIORef radioS $ const False
        element radioBtn # set UI.text "Start Radio"
        return ()
      defHandler RecordingStart = do
        rS <- liftIO . readIORef $ radioS
        when rS $ do
          fire e (ChangeMessage "Recording Started")
          liftIO . modifyIORef recordingS $ const True
          element recordingBtn # set UI.text "Stop Recording"
          return ()
      defHandler RecordingStop = do
        rS <- liftIO . readIORef $ radioS
        when rS $ do
          fire e (ChangeMessage "Recording Stopped")
          liftIO . modifyIORef recordingS $ const False
          element recordingBtn # set UI.text "Start Recording"
          return ()
      defHandler (ChangeMessage m) = do
        liftIO . modifyIORef messageLogS $ (++) (m ++ "\n")
        ms <- liftIO . readIORef $ messageLogS
        element messageLog # set UI.text ms
        return ()
      defHandler VolUp = do
        rS <- liftIO . readIORef $ radioS
        vS <- liftIO . readIORef $ volS
        when (rS && vS < 15) $ do
          fire e (ChangeMessage "Volume Up by 1")
          liftIO $ modifyIORef volS (+1)
          element volDis # set UI.text (show $ vS + 1) 
          return ()
      defHandler VolDown = do
        rS <- liftIO . readIORef $ radioS
        vS <- liftIO . readIORef $ volS
        when (rS && vS > 0) $ do
          fire e (ChangeMessage "Volume down by 1")
          liftIO $ modifyIORef volS (flip (-) 1)
          element volDis # set UI.text (show $ vS - 1) 
          return ()
      defHandler (ChangeChannel c) = do
        rS <- liftIO . readIORef $ radioS
        cS <- liftIO . readIORef $ selChannelS
        when (rS && (cS /= c)) $ do
          fire e (ChangeMessage $ "Channel changed to " ++ fst c)
          liftIO $ modifyIORef selChannelS $ const c
          return ()
      defHandler GetChannel = fire e (ChangeMessage "Getting channel")
  
  -- start listening
  listen e defHandler

  -- connect dom events
  on UI.click radioBtn $ const $ do
    x <- liftIO $ readIORef radioS
    if x then fire e RadioStop
         else fire e RadioStart
  on UI.click recordingBtn $ const $ do
    x <- liftIO $ readIORef recordingS
    if x then fire e RecordingStop
         else fire e RecordingStart
  on UI.click volPlus $ const $ fire e VolUp
  on UI.click volMinus $ const $ fire e VolDown
  on UI.selectionChange selectBox $ \x ->
    case x of
      Nothing -> return ()
      Just i -> fire e (ChangeChannel (channels !! i))

  -- display
  let g = grid [
          [element radioBtn, element recordingBtn],
          [element volMinus, element volDis, element volPlus],
          [element selectBox],
          [element messageLog]
        ]

  getBody window #+ [g]
  return ()

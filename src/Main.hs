{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class
import Data.IORef
import qualified Data.IntMap as M
import Data.Unique
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Control.Monad
import Data.Char (isSpace)
import Data.Aeson
import Network.HTTP.Simple
import Network.HTTP.Conduit
import Models

-- The main function
main :: IO ()
main = startGUI defaultConfig {jsPort = Just 8023} setup

-- A simple event handler
data EHandler a = EHandler
  { listen :: (a -> UI ()) -> UI ()
  , fire :: a -> UI ()
  }

-- The events in the entire application
data EType
  = RadioStart
  | RadioStop
  | RecordingStart
  | RecordingStop
  | LogMessage String
  | VolUp
  | VolDown
  | ChangeChannel Channel
  | GetChannel
  | RecordingNameChange String

type Channel = (String, Double)

-- Http helper for posting data
postJson :: (ToJSON a, FromJSON b) => String -> a -> UI b 
postJson url a = do
  request' <- parseRequest url
  let request = setRequestMethod "POST"
              $ setRequestBodyLBS (encode a)
              $ request'
  getResponseBody <$> httpJSON request

-- Http helper for getting data
getJson :: (FromJSON b) => String -> UI b 
getJson url = do
  request' <- parseRequest url
  let request = setRequestMethod "GET"
              $ request'
  getResponseBody <$> httpJSON request

-- Helper function trim
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

-- Helper function tSpan
tSpan :: String -> UI Element
tSpan t = UI.span # set UI.text t # set UI.style [("padding", "5px")]

-- EHandler generator
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

-- Defined channels
channels :: [(String, Double)]
channels = [("OE3", 99.90), ("Kronehit", 105.80), ("FM4", 103.80), ("NRJ", 104.20), ("Mein Kinderradio", 103.20)]

-- Base folder to save recordings, note the trailing '/'
baseFolder :: String
baseFolder = "/C/"

-- Main setting up
setup :: Window -> UI ()
setup window
 = do

  -- Window properties
  pure window # set UI.title "Radio Tuner"

  -- State
  radioS <- liftIO $ newIORef False
  recordingS <- liftIO $ newIORef False
  messageLogS <- liftIO $ newIORef ("" :: String)
  volS <- liftIO $ newIORef (7 :: Int)
  selChannelS <- liftIO $ newIORef (head channels :: Channel)
  recordingNameS <- liftIO $ newIORef ("" :: String)

  -- Register event handler
  (e :: EHandler EType) <- newEHandler

  -- UI Elements
  radioBtn <- UI.button # set UI.text "Start Radio"
  recordingBtn <- UI.button # set UI.text "Start Recording"
  messageLog <- UI.pre # set UI.text "Log is displayed here"
                       # set UI.style [("max-height", "200px"), ("overflow", "auto"), ("overflow-x", "hidden")]
  volDis <- tSpan "7"
  volPlus <- UI.button # set UI.text "[ + ]"
  volMinus <- UI.button # set UI.text "[ - ]"
  selectBox <- createSelectBox $ map (\(a, b) -> (a, show b)) channels
  recordingName <- UI.input # set UI.type_ "text"
  getChannelBtn <- UI.button # set UI.text "Get Channel"

  -- Main event handler function
  -- Event handler for RadioStart
  let defHandler RadioStart = do
        fire e (LogMessage "Radio Started")
        liftIO . modifyIORef radioS $ const True
        element radioBtn # set UI.text "Stop Radio"
        return ()

  -- Event handler for RadioStop
      defHandler RadioStop = do
        fire e RecordingStop
        fire e (LogMessage "Radio Stopped")
        liftIO . modifyIORef radioS $ const False
        element radioBtn # set UI.text "Start Radio"
        return ()

    -- Event handler for RecordingStart
      defHandler RecordingStart = do
        rS <- liftIO . readIORef $ radioS
        nS <- trim <$> (liftIO . readIORef $ recordingNameS)
        when (rS && (nS /= "")) $ do
          -- HTTP begin
          r <- postJson "URL_HERE" $ Recording nS
          fire e (LogMessage $ _message r)
          -- HTTP end
          fire e (LogMessage "Recording Started")
          let rP = baseFolder ++ nS
          fire e (LogMessage $ "Recording being saved to '" ++ rP ++ "'")
          liftIO . modifyIORef recordingS $ const True
          element recordingBtn # set UI.text "Stop Recording"
          return ()

    -- Event handler for RecordingStop
      defHandler RecordingStop = do
        rS <- liftIO . readIORef $ radioS
        when rS $ do
          -- HTTP begin
          -- Make some HTTP request
          -- HTTP end
          fire e (LogMessage "Recording Stopped")
          liftIO . modifyIORef recordingS $ const False
          element recordingBtn # set UI.text "Start Recording"
          return ()

    -- Event handler for LogMessage
      defHandler (LogMessage m) = do
        liftIO . modifyIORef messageLogS $ (++) (m ++ "\n")
        ms <- liftIO . readIORef $ messageLogS
        element messageLog # set UI.text ms
        return ()

    -- Event handler for VolUp
      defHandler VolUp = do
        rS <- liftIO . readIORef $ radioS
        vS <- liftIO . readIORef $ volS
        when (rS && vS < 15) $ do
          -- HTTP start
          r <- postJson "URL_HERE" $ VolumeRequest (vS + 1)
          fire e (LogMessage $ _message r)
         -- HTTP stop
          fire e (LogMessage "Volume Up by 1")
          liftIO $ modifyIORef volS (+1)
          element volDis # set UI.text (show $ vS + 1) 
          return ()

    -- Event handler for VolDown
      defHandler VolDown = do
        rS <- liftIO . readIORef $ radioS
        vS <- liftIO . readIORef $ volS
        when (rS && vS > 0) $ do
          -- HTTP begin
          r <- postJson "URL_HERE" $ VolumeRequest (vS - 1)
          fire e (LogMessage $ _message r)
          -- HTTP end
          fire e (LogMessage "Volume down by 1")
          liftIO $ modifyIORef volS (flip (-) 1)
          element volDis # set UI.text (show $ vS - 1) 
          return ()

    -- Event handler for ChangeChannel
      defHandler (ChangeChannel c) = do
        rS <- liftIO . readIORef $ radioS
        cS <- liftIO . readIORef $ selChannelS
        when (rS && (cS /= c)) $ do
          -- HTTP start
          r <- postJson "URL_HERE" $ RequestChannel (snd c)
          fire e (LogMessage $ "The Channel is " ++ _channel r)
          -- HTTP stop
          fire e (LogMessage $ "Channel changed to " ++ fst c)
          liftIO $ modifyIORef selChannelS $ const c
          return ()

    -- Event handler for GetChannel
      defHandler GetChannel = do
        rS <- liftIO . readIORef $ radioS
        when rS $ do
          -- HTTP start
          fire e (LogMessage "Getting channel")
          c <- getJson "http://www.mocky.io/v2/5d0d81dd3400006300ca4a1d"
          -- HTTP stop
          fire e (LogMessage $ "The Channel is " ++ _channel c)
          return ()

    -- Event handler for RecordingNameChange
      defHandler (RecordingNameChange s) = do
        rS <- liftIO . readIORef $ radioS
        when rS $ do
          fire e (LogMessage $ "Recording name changed to " ++ s)
          liftIO $ modifyIORef recordingNameS $ const s
          return ()
  
  -- Start listening
  listen e defHandler

  -- Connect dom events
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
  on UI.click getChannelBtn $ const $ fire e GetChannel
  on UI.valueChange recordingName $ fire e . RecordingNameChange
  on UI.selectionChange selectBox $ \x ->
    case x of
      Nothing -> return ()
      Just i -> fire e (ChangeChannel (channels !! i))

  -- Display
  let g = grid [
           [tSpan "Radio Control", element radioBtn],
           [tSpan "Current Channel", element getChannelBtn],
           [tSpan "Selected Channel", element selectBox],
           [tSpan "Volume Control", UI.div #+ 
             [element volMinus, element volDis, element volPlus]],
           [tSpan "Recording Name", element recordingName],
           [tSpan "Recording Control", element recordingBtn],
           [tSpan "Log", element messageLog]
        ]

  getBody window #+ [g]
  return ()

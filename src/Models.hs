{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Models where

import Data.Aeson
import GHC.Generics

newtype RequestChannel = RequestChannel
  { _frequenz :: Double
  } deriving (Show, Generic)

data ResponseChannel = ResponseChannel
  { _channel :: !String
  } deriving (Show, Generic)

data Recording = Recording
  { _filePath :: !String
  } deriving (Show, Generic)

newtype VolumeRequest = VolumeRequest
  { _volume :: Int
  } deriving (Show, Generic)

instance ToJSON RequestChannel

instance FromJSON ResponseChannel

instance ToJSON Recording

instance ToJSON VolumeRequest

instance FromJSON VolumeRequest

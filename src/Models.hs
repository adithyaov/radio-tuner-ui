
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
module Dtos where

import GHC.Generics
import Data.Aeson
import Control.Lens
import Snap.Snaplet.PostgresqlSimple

data RequestChannel = RequestChannel { _frequenz  :: Double} deriving (Show, Generic)
data ResponseChannel = ResponseChannel { _channel  :: ![Char]} deriving (Show, Generic)
data Recording = Recording { _filePath  :: ![Char]} deriving (Show, Generic)
data VolumeRequest = VolumeRequest { _volume  :: Double} deriving (Show, Generic)


makeLenses ''RequestChannel
makeLenses ''ResponseChannel
makeLenses ''Recording
makeLenses ''VolumeRequest

instance FromJSON RequestChannel
instance ToJSON ResponseChannel
instance FromJSON Recording
instance ToJSON Recording
instance FromJSON VolumeRequest

instance FromRow Recording where
  fromRow = Recording <$> field


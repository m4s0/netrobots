{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Model.EventDrive
    ( EventDrive (..)
    ) where

import Data.Aeson
import GHC.Generics
import Test.QuickCheck
import Model.Event
import Model.Number
import Model.RobotInfo


data EventDrive = EventDrive
    { eventType :: Number
    , activationTime :: Float
    , robot :: RobotInfo
    } deriving (Show, Eq, Generic)

instance FromJSON EventDrive
instance ToJSON EventDrive
instance Arbitrary EventDrive where
    arbitrary = EventDrive <$> arbitrary <*> arbitrary <*> arbitrary

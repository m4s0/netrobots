{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Model.EventScan
    ( EventScan (..)
    ) where

import Data.Aeson
import GHC.Generics
import Test.QuickCheck
import Model.Event
import Model.Number
import Model.RobotInfo


data EventScan = EventScan
    { eventType :: Number
    , activationTime :: Float
    , direction :: Float
    , semiaperture :: Float
    , scanMaxDistance :: Float
    , robot :: RobotInfo
    , hitRobot :: RobotInfo
    } deriving (Show, Eq, Generic)

instance FromJSON EventScan
instance ToJSON EventScan
instance Arbitrary EventScan where
    arbitrary = EventScan <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

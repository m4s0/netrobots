{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Model.EventCreateRobot
    ( EventCreateRobot (..)
    ) where

import Data.Aeson
import GHC.Generics
import Test.QuickCheck
import Model.Event
import Model.Number
import Model.RobotInfo


data EventCreateRobot = EventCreateRobot
    { eventType :: Number
    , activationTime :: Float
    , robot :: RobotInfo
    , name :: String
    , color :: String
    } deriving (Show, Eq, Generic)

instance FromJSON EventCreateRobot
instance ToJSON EventCreateRobot
instance Arbitrary EventCreateRobot where
    arbitrary = EventCreateRobot <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

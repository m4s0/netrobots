{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Model.EventRemoveRobot
    ( EventRemoveRobot (..)
    ) where

import Data.Aeson
import GHC.Generics
import Test.QuickCheck
import Model.Event
import Model.Number
import Model.RobotInfo


data EventRemoveRobot = EventRemoveRobot
    { eventType :: Number
    , activationTime :: Float
    , robot :: RobotInfo
    } deriving (Show, Eq, Generic)

instance FromJSON EventRemoveRobot
instance ToJSON EventRemoveRobot
instance Arbitrary EventRemoveRobot where
    arbitrary = EventRemoveRobot <$> arbitrary <*> arbitrary <*> arbitrary

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Model.EventRobotCollision
    ( EventRobotCollision (..)
    ) where

import Data.Aeson
import GHC.Generics
import Test.QuickCheck
import Model.Event
import Model.Number
import Model.RobotInfo


data EventRobotCollision = EventRobotCollision
    { eventType :: Number
    , activationTime :: Float
    , robot :: RobotInfo
    } deriving (Show, Eq, Generic)

instance FromJSON EventRobotCollision
instance ToJSON EventRobotCollision
instance Arbitrary EventRobotCollision where
    arbitrary = EventRobotCollision <$> arbitrary <*> arbitrary <*> arbitrary

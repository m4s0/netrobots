{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Model.EventExplosion
    ( EventExplosion (..)
    ) where

import Data.Aeson
import GHC.Generics
import Test.QuickCheck
import Model.Event
import Model.Number
import Model.RobotInfo


data EventExplosion = EventExplosion
    { eventType :: Number
    , activationTime :: Float
    , robot :: RobotInfo
    , hitRobot :: RobotInfo
    , damage :: Float
    } deriving (Show, Eq, Generic)

instance FromJSON EventExplosion
instance ToJSON EventExplosion
instance Arbitrary EventExplosion where
    arbitrary = EventExplosion <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

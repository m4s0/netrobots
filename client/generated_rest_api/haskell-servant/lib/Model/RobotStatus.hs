{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Model.RobotStatus
    ( RobotStatus (..)
    ) where

import Data.Aeson
import GHC.Generics
import Test.QuickCheck
import Model.RobotConfiguration
import Model.ScanStatus


data RobotStatus = RobotStatus
    { name :: String
    , token :: String
    , configuration :: RobotConfiguration
    , simulationTime :: Float
    , timeTick :: Float
    , realTimeTick :: Float
    , health :: Float
    , isDead :: Bool
    , isWinner :: Bool
    , isWellSpecifiedRobot :: Bool
    , direction :: Float
    , speed :: Float
    , posX :: Float
    , posY :: Float
    , cannonReloadingTime :: Float
    , firedNewMissile :: Bool
    , scanStatus :: ScanStatus
    } deriving (Show, Eq, Generic)

instance FromJSON RobotStatus
instance ToJSON RobotStatus
instance Arbitrary RobotStatus where
    arbitrary = RobotStatus <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

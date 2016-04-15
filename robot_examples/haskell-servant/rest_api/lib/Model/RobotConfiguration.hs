{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Model.RobotConfiguration
    ( RobotConfiguration (..)
    ) where

import Data.Aeson
import GHC.Generics
import Test.QuickCheck


data RobotConfiguration = RobotConfiguration
    { name :: String
    , maxHitPoints :: Float
    , maxSpeed :: Float
    , acceleration :: Float
    , decelleration :: Float
    , maxSterlingSpeed :: Float
    , maxScanDistance :: Float
    , maxFireDistance :: Float
    , bulletSpeed :: Float
    , bulletDamage :: Float
    , cannonReloadingTime :: Float
    } deriving (Show, Eq, Generic)

instance FromJSON RobotConfiguration
instance ToJSON RobotConfiguration
instance Arbitrary RobotConfiguration where
    arbitrary = RobotConfiguration <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Model.EventMissile
    ( EventMissile (..)
    ) where

import Data.Aeson
import GHC.Generics
import Test.QuickCheck
import Model.Event
import Model.Number
import Model.RobotInfo


data EventMissile = EventMissile
    { eventType :: Number
    , activationTime :: Float
    , robot :: RobotInfo
    , direction :: Float
    , distance :: Float
    , speed :: Float
    } deriving (Show, Eq, Generic)

instance FromJSON EventMissile
instance ToJSON EventMissile
instance Arbitrary EventMissile where
    arbitrary = EventMissile <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

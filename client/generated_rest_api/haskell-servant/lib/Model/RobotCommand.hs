{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Model.RobotCommand
    ( RobotCommand (..)
    ) where

import Data.Aeson
import GHC.Generics
import Test.QuickCheck
import Model.DriveCommand
import Model.FireCommand
import Model.ScanCommand


data RobotCommand = RobotCommand
    { token :: String
    , fire :: FireCommand
    , drive :: DriveCommand
    , scan :: ScanCommand
    } deriving (Show, Eq, Generic)

instance FromJSON RobotCommand
instance ToJSON RobotCommand
instance Arbitrary RobotCommand where
    arbitrary = RobotCommand <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

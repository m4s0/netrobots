{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Model.DriveCommand
    ( DriveCommand (..)
    ) where

import Data.Aeson
import GHC.Generics
import Test.QuickCheck


data DriveCommand = DriveCommand
    { direction :: Float
    , speed :: Float
    } deriving (Show, Eq, Generic)

instance FromJSON DriveCommand
instance ToJSON DriveCommand
instance Arbitrary DriveCommand where
    arbitrary = DriveCommand <$> arbitrary <*> arbitrary

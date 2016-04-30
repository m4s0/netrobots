{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Model.ScanCommand
    ( ScanCommand (..)
    ) where

import Data.Aeson
import GHC.Generics
import Test.QuickCheck


data ScanCommand = ScanCommand
    { direction :: Float
    , semiaperture :: Float
    } deriving (Show, Eq, Generic)

instance FromJSON ScanCommand
instance ToJSON ScanCommand
instance Arbitrary ScanCommand where
    arbitrary = ScanCommand <$> arbitrary <*> arbitrary

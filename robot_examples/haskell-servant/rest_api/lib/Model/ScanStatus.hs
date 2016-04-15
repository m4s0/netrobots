{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Model.ScanStatus
    ( ScanStatus (..)
    ) where

import Data.Aeson
import GHC.Generics
import Test.QuickCheck


data ScanStatus = ScanStatus
    { direction :: Float
    , semiApertureAngle :: Float
    , distance :: Float
    } deriving (Show, Eq, Generic)

instance FromJSON ScanStatus
instance ToJSON ScanStatus
instance Arbitrary ScanStatus where
    arbitrary = ScanStatus <$> arbitrary <*> arbitrary <*> arbitrary

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Model.FireCommand
    ( FireCommand (..)
    ) where

import Data.Aeson
import GHC.Generics
import Test.QuickCheck


data FireCommand = FireCommand
    { direction :: Float
    , distance :: Float
    } deriving (Show, Eq, Generic)

instance FromJSON FireCommand
instance ToJSON FireCommand
instance Arbitrary FireCommand where
    arbitrary = FireCommand <$> arbitrary <*> arbitrary

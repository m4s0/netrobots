{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Model.Event
    ( Event (..)
    ) where

import Data.Aeson
import GHC.Generics
import Test.QuickCheck
import Model.Number


data Event = Event
    { eventType :: Number
    , activationTime :: Float
    } deriving (Show, Eq, Generic)

instance FromJSON Event
instance ToJSON Event
instance Arbitrary Event where
    arbitrary = Event <$> arbitrary <*> arbitrary

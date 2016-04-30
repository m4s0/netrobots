{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Model.RemoveCommand
    ( RemoveCommand (..)
    ) where

import Data.Aeson
import GHC.Generics
import Test.QuickCheck


data RemoveCommand = RemoveCommand
    { token :: String
    } deriving (Show, Eq, Generic)

instance FromJSON RemoveCommand
instance ToJSON RemoveCommand
instance Arbitrary RemoveCommand where
    arbitrary = RemoveCommand <$> arbitrary

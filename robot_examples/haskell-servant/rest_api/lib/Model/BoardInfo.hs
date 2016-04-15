{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Model.BoardInfo
    ( BoardInfo (..)
    ) where

import Data.Aeson
import GHC.Generics
import Test.QuickCheck
import Model.Event


data BoardInfo = BoardInfo
    { maxBoardX :: Float
    , maxBoardY :: Float
    , streamDelay :: Float
    , turnDeltaTime :: Float
    , networkLatency :: Float
    , startTime :: Float
    , endTime :: Float
    , events :: [Event]
    } deriving (Show, Eq, Generic)

instance FromJSON BoardInfo
instance ToJSON BoardInfo
instance Arbitrary BoardInfo where
    arbitrary = BoardInfo <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

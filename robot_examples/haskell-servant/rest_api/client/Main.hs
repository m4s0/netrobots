{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad (void)
import Control.Monad.Trans.Either
import Control.Monad.IO.Class
import Servant.API
import Servant.Client

import Data.List.Split (splitOn)
import Network.URI (URI (..), URIAuth (..), parseURI)
import Data.Maybe (fromMaybe)
import Test.QuickCheck
import Control.Monad
import Model.BoardInfo
import Model.DriveCommand
import Model.Event
import Model.EventCreateRobot
import Model.EventDrive
import Model.EventExplosion
import Model.EventMissile
import Model.EventRemoveRobot
import Model.EventRobotCollision
import Model.EventScan
import Model.FireCommand
import Model.RobotCommand
import Model.RobotConfiguration
import Model.RobotInfo
import Model.RobotStatus
import Model.ScanCommand
import Model.ScanStatus
import Api.DefaultApi

-- userClient :: IO ()
-- userClient = do 
--     users <- sample' (arbitrary :: Gen String)
--     let user = last users
--     void . runEitherT $ do
--         getUserByName user >>= (liftIO . putStrLn . show)

main :: IO ()
main = putStrLn "Hello Server!"

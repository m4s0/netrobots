{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.DefaultApi (
      robotActionPost
    , robotCreatePost
    , robotRemovePost
    , proxyDefaultApi
    , DefaultApi
    ) where

import GHC.Generics
import Data.Proxy
import Servant.API
import Servant.Client
import Network.URI (URI (..), URIAuth (..), parseURI)
import Data.Maybe (fromMaybe)
import Servant.Common.Text
import Data.List (intercalate)
import qualified Data.Text as T
import Utils
import Test.QuickCheck
import Model.RobotCommand
import Model.RobotStatus
import Model.RobotConfiguration
import Model.RemoveCommand





type DefaultApi = "robot-action" :> ReqBody '[JSON] RobotCommand :> Post '[JSON] RobotStatus -- robotActionPost
    :<|> "robot-create" :> ReqBody '[JSON] RobotConfiguration :> Post '[JSON] RobotStatus -- robotCreatePost
    :<|> "robot-remove" :> ReqBody '[JSON] RemoveCommand :> Post '[JSON] () -- robotRemovePost

proxyDefaultApi :: Proxy DefaultApi
proxyDefaultApi = Proxy


serverPath :: String
serverPath = "http://localhost"

parseHostPort :: String -> (String, Int)
parseHostPort path = (host,port)
    where
        authority = case parseURI path of
            Just x -> uriAuthority x
            _      -> Nothing
        (host, port) = case authority of
            Just y -> (uriRegName y, (getPort . uriPort) y)
            _      -> ("localhost", 8080)
        getPort p = case (length p) of
            0 -> 80
            _ -> (read . drop 1) p

(host, port) = parseHostPort serverPath

robotActionPost
    :<|> robotCreatePost
    :<|> robotRemovePost
    = client proxyDefaultApi $ BaseUrl Http host port

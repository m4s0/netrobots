{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.DefaultApi (
      boardEventsGet
    , boardInfoGet
    , robotActionPost
    , robotCreatePost
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
import Model.BoardInfo
import Model.RobotCommand
import Model.RobotStatus
import Model.RobotConfiguration






type DefaultApi = "board-events" :> Get '[JSON] BoardInfo -- boardEventsGet
    :<|> "board-info" :> Get '[JSON] BoardInfo -- boardInfoGet
    :<|> "robot-action" :> ReqBody '[JSON] RobotCommand :> Post '[JSON] RobotStatus -- robotActionPost
    :<|> "robot-create" :> ReqBody '[JSON] RobotConfiguration :> Post '[JSON] RobotStatus -- robotCreatePost

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

boardEventsGet
    :<|> boardInfoGet
    :<|> robotActionPost
    :<|> robotCreatePost
    = client proxyDefaultApi $ BaseUrl Http host port

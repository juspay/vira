{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Attic types
module Attic.Types (
  AtticServer (..),
  AtticToken (..),
  AtticCache (..),
  AtticServerEndpoint (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.SafeCopy
import Servant.API (FromHttpApiData, ToHttpApiData)
import TOML (DecodeTOML)
import Web.FormUrlEncoded (FromForm)

-- | Attic server endpoint URL
newtype AtticServerEndpoint = AtticServerEndpoint {unAtticServerEndpoint :: Text}
  deriving stock (Eq, Show)
  deriving newtype
    ( IsString
    , ToString
    , ToText
    , ToHttpApiData
    , FromHttpApiData
    , ToJSON
    , FromJSON
    , DecodeTOML
    )

$(deriveSafeCopy 0 'base ''AtticServerEndpoint)

-- | Reference to a self-hosted attic server
data AtticServer = AtticServer
  { name :: Text
  , endpoint :: AtticServerEndpoint
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromForm, ToJSON, FromJSON)

-- | An attic login token
newtype AtticToken = AtticToken {unAtticToken :: Text}
  deriving stock (Eq, Show)
  deriving newtype
    ( IsString
    , ToString
    , ToHttpApiData
    , FromHttpApiData
    , ToJSON
    , FromJSON
    , DecodeTOML
    )

-- | An attic cache name
newtype AtticCache = AtticCache {unAtticCache :: Text}
  deriving stock (Eq, Show)
  deriving newtype
    ( IsString
    , ToString
    , ToText
    , ToHttpApiData
    , FromHttpApiData
    , ToJSON
    , FromJSON
    )

$(deriveSafeCopy 0 'base ''AtticServer)
$(deriveSafeCopy 0 'base ''AtticCache)
$(deriveSafeCopy 0 'base ''AtticToken)

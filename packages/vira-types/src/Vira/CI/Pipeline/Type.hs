{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module Vira.CI.Pipeline.Type where

import Optics.TH

-- | CI Pipeline configuration types
data ViraPipeline = ViraPipeline
  { build :: BuildStage
  , attic :: AtticStage
  , cachix :: CachixStage
  , signoff :: SignoffStage
  }
  deriving stock (Generic, Show)

data BuildStage = BuildStage
  { buildEnable :: Bool
  , overrideInputs :: [(Text, Text)]
  }
  deriving stock (Generic, Show)

newtype AtticStage = AtticStage
  { atticEnable :: Bool
  }
  deriving stock (Generic, Show)

newtype CachixStage = CachixStage
  { cachixEnable :: Bool
  }
  deriving stock (Generic, Show)

newtype SignoffStage = SignoffStage
  { signoffEnable :: Bool
  }
  deriving stock (Generic, Show)

makeLenses ''ViraPipeline
makeLenses ''BuildStage
makeLenses ''SignoffStage

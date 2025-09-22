{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedRecordUpdate #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module Vira.CI.Pipeline.Type where

import Optics.TH
import Relude (Bool (..), Generic, Show, Text)
import GHC.Records.Compat

-- | CI Pipeline configuration types
data ViraPipeline = ViraPipeline
  { build :: BuildStage
  , attic :: AtticStage
  , cachix :: CachixStage
  , signoff :: SignoffStage
  }
  deriving stock (Generic, Show)

data BuildStage = BuildStage
  { enable :: Bool
  , overrideInputs :: [(Text, Text)]
  }
  deriving stock (Generic, Show)

newtype AtticStage = AtticStage
  { enable :: Bool
  }
  deriving stock (Generic, Show)

newtype CachixStage = CachixStage
  { enable :: Bool
  }
  deriving stock (Generic, Show)

newtype SignoffStage = SignoffStage
  { enable :: Bool
  }
  deriving stock (Generic, Show)

instance HasField "enable" SignoffStage Bool where
  hasField (SignoffStage enable) = (SignoffStage, enable)

instance HasField "signoff" ViraPipeline SignoffStage where
   hasField r = (\x -> r{ signoff = x } , r.signoff)

-- TODO: Implement the necessary instances, and demo updating all kinds of nested fields of the pipeline
demo :: ViraPipeline -> ViraPipeline
demo r =
  r {signoff.enable = True}

makeFieldLabelsNoPrefix ''ViraPipeline
makeFieldLabelsNoPrefix ''BuildStage
makeFieldLabelsNoPrefix ''AtticStage
makeFieldLabelsNoPrefix ''CachixStage
makeFieldLabelsNoPrefix ''SignoffStage

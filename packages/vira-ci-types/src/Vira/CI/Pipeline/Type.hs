{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedRecordUpdate #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Avoid lambda" #-}

module Vira.CI.Pipeline.Type where

import GHC.Records.Compat
import Relude (Bool (..), Generic, Show, Text)

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

-- HasField instances for enabling OverloadedRecordUpdate syntax (see vira.hs)
-- NOTE: Do not forgot to fill in these instances if the types above change.
-- In future, we could generically derive them using generics-sop and the like.

instance HasField "enable" BuildStage Bool where
  hasField (BuildStage enable overrideInputs) = ((`BuildStage` overrideInputs), enable)

instance HasField "overrideInputs" BuildStage [(Text, Text)] where
  hasField (BuildStage enable overrideInputs) = (BuildStage enable, overrideInputs)

instance HasField "enable" AtticStage Bool where
  hasField (AtticStage enable) = (AtticStage, enable)

instance HasField "enable" CachixStage Bool where
  hasField (CachixStage enable) = (CachixStage, enable)

instance HasField "enable" SignoffStage Bool where
  hasField (SignoffStage enable) = (SignoffStage, enable)

instance HasField "build" ViraPipeline BuildStage where
  hasField (ViraPipeline build attic cachix signoff) = (\x -> ViraPipeline x attic cachix signoff, build)

instance HasField "attic" ViraPipeline AtticStage where
  hasField (ViraPipeline build attic cachix signoff) = (\x -> ViraPipeline build x cachix signoff, attic)

instance HasField "cachix" ViraPipeline CachixStage where
  hasField (ViraPipeline build attic cachix signoff) = (\x -> ViraPipeline build attic x signoff, cachix)

instance HasField "signoff" ViraPipeline SignoffStage where
  hasField (ViraPipeline build attic cachix signoff) = (ViraPipeline build attic cachix, signoff)

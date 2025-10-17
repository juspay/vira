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
import Relude (Bool (..), Generic, Maybe, NonEmpty, Show, Text)

-- | CI Pipeline configuration types
data ViraPipeline = ViraPipeline
  { build :: BuildStage
  , cache :: CacheStage
  , signoff :: SignoffStage
  }
  deriving stock (Generic, Show)

newtype BuildStage = BuildStage
  { flakes :: NonEmpty Flake
  }
  deriving stock (Generic, Show)

-- | Configuration for building a flake at a specific path
data Flake = Flake
  { path :: Text
  , overrideInputs :: [(Text, Text)]
  }
  deriving stock (Generic, Show)

newtype SignoffStage = SignoffStage
  { enable :: Bool
  }
  deriving stock (Generic, Show)

-- TODO: Switch url type to URI from modern-uri for better type safety
newtype CacheStage = CacheStage
  { url :: Maybe Text
  }
  deriving stock (Generic, Show)

-- HasField instances for enabling OverloadedRecordUpdate syntax (see vira.hs)
-- NOTE: Do not forgot to fill in these instances if the types above change.
-- In future, we could generically derive them using generics-sop and the like.

instance HasField "path" Flake Text where
  hasField (Flake path overrideInputs) = (\x -> Flake x overrideInputs, path)

instance HasField "overrideInputs" Flake [(Text, Text)] where
  hasField (Flake path overrideInputs) = (Flake path, overrideInputs)

instance HasField "flakes" BuildStage (NonEmpty Flake) where
  hasField (BuildStage flakes) = (BuildStage, flakes)

instance HasField "enable" SignoffStage Bool where
  hasField (SignoffStage enable) = (SignoffStage, enable)

instance HasField "url" CacheStage (Maybe Text) where
  hasField (CacheStage url) = (CacheStage, url)

instance HasField "build" ViraPipeline BuildStage where
  hasField (ViraPipeline build cache signoff) = (\x -> ViraPipeline x cache signoff, build)

instance HasField "cache" ViraPipeline CacheStage where
  hasField (ViraPipeline build cache signoff) = (\x -> ViraPipeline build x signoff, cache)

instance HasField "signoff" ViraPipeline SignoffStage where
  hasField (ViraPipeline build cache signoff) = (ViraPipeline build cache, signoff)

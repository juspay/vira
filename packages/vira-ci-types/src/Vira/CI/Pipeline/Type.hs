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
{-# HLINT ignore "Use 'fromString' from Relude" #-}

module Vira.CI.Pipeline.Type where

import Data.String (IsString (..))
import GHC.Records.Compat
import Relude (Bool (..), FilePath, Generic, Maybe, NonEmpty, Show, Text, notElem)
import System.Nix.System (System)

-- | CI Pipeline configuration types
data ViraPipeline = ViraPipeline
  { build :: BuildStage
  , nix :: NixConfig
  , cache :: CacheStage
  , signoff :: SignoffStage
  }
  deriving stock (Generic, Show)

data BuildStage = BuildStage
  { flakes :: NonEmpty Flake
  , systems :: [System]
  }
  deriving stock (Generic, Show)

-- | Nix-level configuration (options and experimental features)
newtype NixConfig = NixConfig
  { options :: [(Text, Text)]
  {- ^ Nix @--option key value@ flags. Only whitelisted keys are allowed;
    see 'allowedNixOptions'.
  -}
  }
  deriving stock (Generic, Show)

{- | Whitelist of Nix option keys that are safe to set per-project.

Secrets (like @access-tokens@) must NOT be added here — they belong
in @nix.conf@ on the CI machine, not in @vira.hs@.
-}
allowedNixOptions :: [Text]
allowedNixOptions =
  [ "sandbox" -- e.g. "relaxed" or "false"
  , "cores" -- CPU cores per build
  , "max-jobs" -- parallel build jobs
  , "allow-import-from-derivation" -- IFD control
  ]

{- | Validate that all nix option keys are in the whitelist.
Returns a list of disallowed keys.
-}
validateNixOptions :: [(Text, Text)] -> [Text]
validateNixOptions opts =
  [k | (k, _) <- opts, k `notElem` allowedNixOptions]

-- | Configuration for building a flake at a specific path
data Flake = Flake
  { path :: FilePath
  , overrideInputs :: [(Text, Text)]
  }
  deriving stock (Generic, Show)

{- | Allows using string literals for Flake paths with optional record update

Examples:
  "." :: Flake                                    -- Simple path
  "./doc" { overrideInputs = [...] } :: Flake     -- With overrides
-}
instance IsString Flake where
  fromString s = Flake (fromString s) []

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

instance HasField "path" Flake FilePath where
  hasField (Flake path overrideInputs) = (\x -> Flake x overrideInputs, path)

instance HasField "overrideInputs" Flake [(Text, Text)] where
  hasField (Flake path overrideInputs) = (Flake path, overrideInputs)

instance HasField "flakes" BuildStage (NonEmpty Flake) where
  hasField (BuildStage flakes systems) = (\x -> BuildStage x systems, flakes)

instance HasField "systems" BuildStage [System] where
  hasField (BuildStage flakes systems) = (BuildStage flakes, systems)

instance HasField "options" NixConfig [(Text, Text)] where
  hasField (NixConfig options) = (NixConfig, options)

instance HasField "enable" SignoffStage Bool where
  hasField (SignoffStage enable) = (SignoffStage, enable)

instance HasField "url" CacheStage (Maybe Text) where
  hasField (CacheStage url) = (CacheStage, url)

instance HasField "build" ViraPipeline BuildStage where
  hasField (ViraPipeline build nix cache signoff) = (\x -> ViraPipeline x nix cache signoff, build)

instance HasField "nix" ViraPipeline NixConfig where
  hasField (ViraPipeline build nix cache signoff) = (\x -> ViraPipeline build x cache signoff, nix)

instance HasField "cache" ViraPipeline CacheStage where
  hasField (ViraPipeline build nix cache signoff) = (\x -> ViraPipeline build nix x signoff, cache)

instance HasField "signoff" ViraPipeline SignoffStage where
  hasField (ViraPipeline build nix cache signoff) = (ViraPipeline build nix cache, signoff)

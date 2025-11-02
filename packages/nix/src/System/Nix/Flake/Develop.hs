-- | Working with @nix develop@
module System.Nix.Flake.Develop (
  inNixShell,
) where

{- | Check if running inside a @nix develop@ shell

This function checks for the @IN_NIX_SHELL@ environment variable,
which is set by @nix develop@.
-}
inNixShell :: (MonadIO m) => m Bool
inNixShell = do
  isJust <$> lookupEnv "IN_NIX_SHELL"

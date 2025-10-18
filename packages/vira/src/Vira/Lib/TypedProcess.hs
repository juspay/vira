{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | An effect for running child processes using the typed-process library.
module Vira.Lib.TypedProcess (
  -- * Effect
  TypedProcess,

  -- * Handlers
  runTypedProcess,

  -- * Running sub-processes
  startProcess,
  stopProcess,
  readProcessStdout_,
) where

import Effectful
import Effectful.Dispatch.Static
import System.Process.Typed qualified as P

-- | An effect for running child processes using the typed-process library.
data TypedProcess :: Effect

type instance DispatchOf TypedProcess = 'Static 'WithSideEffects
data instance StaticRep TypedProcess = TypedProcess

-- | Run the 'TypedProcess' effect.
runTypedProcess :: (IOE :> es) => Eff (TypedProcess : es) a -> Eff es a
runTypedProcess = evalStaticRep TypedProcess

-- | Start a process.
startProcess ::
  (TypedProcess :> es) =>
  P.ProcessConfig stdin stdout stderr ->
  Eff es (P.Process stdin stdout stderr)
startProcess = unsafeEff_ . P.startProcess

-- | Stop a process.
stopProcess ::
  (TypedProcess :> es) =>
  P.Process stdin stdout stderr ->
  Eff es ()
stopProcess = unsafeEff_ . P.stopProcess

-- | Read the standard output of a process.
readProcessStdout_ ::
  (TypedProcess :> es) =>
  P.ProcessConfig stdin stdout stderr ->
  Eff es LByteString
readProcessStdout_ = unsafeEff_ . P.readProcessStdout_

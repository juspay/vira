{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Beam related effects
module Vira.App.Beam where

import Database.Beam.Sqlite (SqliteM, runBeamSqliteDebug)
import Database.SQLite.Simple qualified as SQLite
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.TH
import Vira.App.Logging

data BeamSqlite :: Effect where
  -- | Get the sqlite connection opened by the app
  AskConn :: BeamSqlite m SQLite.Connection
  -- | Call `runBeamSqliteDebug`
  RunBeam :: SqliteM a -> BeamSqlite m a

makeEffect ''BeamSqlite

runBeamSqlite :: (IOE :> es) => SQLite.Connection -> Eff (BeamSqlite : es) a -> Eff es a
runBeamSqlite conn = interpret $ \_ -> \case
  AskConn -> pure conn
  RunBeam f -> do
    let logBeam _s = runViraLog $ do
          -- log Debug s
          pass
    liftIO $ runBeamSqliteDebug (logBeam . toText) conn f

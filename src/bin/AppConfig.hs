{-# LANGUAGE OverloadedStrings #-}

-- |
-- Here we assemble the actual configuration that Spock needs
-- based on various sources, including our config file.

module AppConfig
  ( SpockServer, SpockConfig, SessionConfig, SessionData(..), AppState
  , getSpockCfg, getSessionCfg
  , loadCfgFile
  )
where

import Data.Pool ( Pool, createPool )
import Data.Configurator as Cfg
import Data.Configurator.Types as Cfg

import qualified Database.PostgreSQL.Simple as PG

import qualified System.Log.FastLogger as Log

import qualified Web.Spock as Spock
import qualified Web.Spock.Config as Spock

data SessionData = EmptySession
type AppState = Log.LoggerSet

type SpockConfig = Spock.SpockCfg PG.Connection SessionData AppState
type SessionConfig = Spock.SessionCfg PG.Connection SessionData AppState
type SpockServer = Spock.SpockM PG.Connection SessionData AppState ()

loadCfgFile :: IO Cfg.Config
loadCfgFile = Cfg.load ["server-configuration.cfg"]

getSpockCfg :: Cfg.Config -> IO SpockConfig
getSpockCfg appCfg = do
  sessionCfg <- getSessionCfg appCfg
  dbPool <- createDBPool appCfg
  loggerSet <- Log.newStderrLoggerSet Log.defaultBufSize
  toplevelCfg <- Spock.defaultSpockCfg EmptySession (Spock.PCPool dbPool) loggerSet
  pure $ toplevelCfg
    { Spock.spc_sessionCfg = sessionCfg }

getSessionCfg :: Cfg.Config -> IO SessionConfig
getSessionCfg appCfg = do
  baseCfg <- Spock.defaultSessionCfg EmptySession

  cookieTTL <- fromInteger <$> Cfg.require appCfg "session.cookie.ttl"
  cookieExpandTTL <- Cfg.require appCfg "session.cookie.expand_ttl"

  pure $ baseCfg
    { Spock.sc_cookieName = "SESSION_ID"
    , Spock.sc_sessionTTL = cookieTTL
    , Spock.sc_sessionExpandTTL = cookieExpandTTL
    }

createDBPool :: Cfg.Config -> IO (Pool PG.Connection)
createDBPool appCfg = do
  poolStripes <- Cfg.require appCfg "db.pool.stripes"
  poolResourceTTL <- fromInteger <$> Cfg.require appCfg "db.pool.resource_ttl"
  connInfo <- connectionInfo appCfg
  createPool
    (PG.connect connInfo)
    PG.close
    poolStripes
    poolResourceTTL
    1                -- maximum resources per pool

connectionInfo :: Cfg.Config -> IO PG.ConnectInfo
connectionInfo appCfg = do
  username <- Cfg.require appCfg "db.user"
  password <- Cfg.require appCfg "db.password"
  host <- Cfg.require appCfg "db.host"
  dbname <- Cfg.require appCfg "db.dbname"
  port <- Cfg.require appCfg "db.port"

  pure $ PG.defaultConnectInfo
    { PG.connectUser = username
    , PG.connectPassword = password
    , PG.connectHost = host
    , PG.connectPort = port
    , PG.connectDatabase = dbname
    }

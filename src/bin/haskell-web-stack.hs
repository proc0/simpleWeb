{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Spock as Spock

import qualified AppConfig as App
import qualified AppHandlers as App

main :: IO ()
main = do
  appCfg <- App.loadCfgFile
  spockCfg <- App.getSpockCfg appCfg
  runSpock 3000 (spock spockCfg server)

server :: App.SpockServer
server = do
  -- Frontend endpoints
  get root $
    App.indexHandler
  get ("dashboard" <//> var) $
    App.dashboardHandler
  get ("js" <//> wildcard) $
    App.jsHandler

  -- API endpoints
  get ("users" <//> var <//> "elements") $
    App.getElementsHandler
  post ("users" <//> var <//> "elements") $
    App.createElementHandler
  delete ("users" <//> var <//> "elements" <//> var) $
    App.deleteElementHandler
  post ("users" <//> var <//> "elements" <//> var <//> "trigger") $
    App.triggerTimerHandler

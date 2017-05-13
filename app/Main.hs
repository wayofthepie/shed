{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment
import System.Remote.Monitoring

import Shed

main :: IO ()
main = do
  args <- getArgs
  forkServer "localhost" 7777
  startApp (args !! 0)

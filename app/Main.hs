{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment
import System.Remote.Monitoring

import Shed

main :: IO ()
main = forkServer "localhost" 7777 >> startApp 

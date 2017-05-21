{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Remote.Monitoring

import Shed

main :: IO ()
main = forkServer "localhost" 7777 >> startApp

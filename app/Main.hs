module Main where

import Options
import Test.Smoke

main :: IO ()
main = do
  options <- parseOptions
  print options
  runSmokeTests undefined undefined

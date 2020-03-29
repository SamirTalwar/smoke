module Test.Smoke.App.Shell
  ( isTTY,
  )
where

isTTY :: IO Bool
isTTY = return False

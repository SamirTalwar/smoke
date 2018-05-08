{-# LANGUAGE OverloadedStrings #-}

module Test.Smoke.App.Diff.DiffUtility
  ( engine
  ) where

import Control.Exception (throwIO)
import qualified Data.ByteString as ByteString
import Data.ByteString.Char8 (unpack)
import Data.Maybe (isJust)
import System.Directory (findExecutable)
import System.Exit (ExitCode(..))
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import System.Process.ByteString (readProcessWithExitCode)
import Test.Smoke.App.Diff.Types

engine :: DiffEngine
engine =
  DiffEngine {engineName = name, engineEnabled = enabled, engineRender = render}

name :: String
name = "diff"

executable :: String
executable = "diff"

enabled :: IO Bool
enabled = isJust <$> findExecutable executable

render :: RenderDiff
render left right =
  withSystemTempFile "smoke-left-" $ \leftFilePath leftFile ->
    withSystemTempFile "smoke-right-" $ \rightFilePath rightFile -> do
      ByteString.hPut leftFile left
      ByteString.hPut rightFile right
      hClose leftFile
      hClose rightFile
      (exitCode, stdout, stderr) <-
        readProcessWithExitCode executable [leftFilePath, rightFilePath] ""
      case exitCode of
        ExitSuccess -> return stdout
        ExitFailure 1 -> return stdout
        ExitFailure code ->
          throwIO $
          userError $
          "`diff` failed with status " ++ show code ++ ".\n" ++ unpack stderr

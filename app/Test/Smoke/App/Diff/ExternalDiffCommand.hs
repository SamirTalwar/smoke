{-# LANGUAGE OverloadedStrings #-}

module Test.Smoke.App.Diff.ExternalDiffCommand
  ( Command
  , enabled
  , render
  ) where

import Control.Exception (throwIO)
import qualified Data.ByteString as ByteString
import Data.ByteString.Char8 (unpack)
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe (isJust)
import System.Directory (findExecutable)
import System.Exit (ExitCode(..))
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import System.Process.ByteString (readProcessWithExitCode)
import Test.Smoke.App.Diff.Types

type Command = NonEmpty String

enabled :: String -> IO Bool
enabled executable = isJust <$> findExecutable executable

render :: Command -> RenderDiff
render (command@(executable :| args)) left right =
  withSystemTempFile "smoke-left-" $ \leftFilePath leftFile ->
    withSystemTempFile "smoke-right-" $ \rightFilePath rightFile -> do
      ByteString.hPut leftFile left
      ByteString.hPut rightFile right
      hClose leftFile
      hClose rightFile
      (exitCode, stdout, stderr) <-
        readProcessWithExitCode
          executable
          (args ++ [leftFilePath, rightFilePath])
          ""
      case exitCode of
        ExitSuccess -> return stdout
        ExitFailure 1 -> return stdout
        ExitFailure code ->
          throwIO $
          userError $
          "`" ++
          unwords (NonEmpty.toList command) ++
          "`" ++
          " failed with status " ++ show code ++ "." ++ "\n" ++ unpack stderr

{-# LANGUAGE OverloadedStrings #-}

module Test.Smoke.App.Diff.ExternalDiffCommand
  ( Command,
    enabled,
    render,
  )
where

import Control.Exception (throwIO)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (isJust)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import System.Directory (findExecutable)
import System.Exit (ExitCode (..))
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import System.Process.Text (readProcessWithExitCode)
import Test.Smoke.App.Diff.Types

type Command = NonEmpty String

enabled :: String -> IO Bool
enabled executable = isJust <$> findExecutable executable

render :: Command -> RenderDiff
render command@(executable :| args) left right =
  withSystemTempFile "smoke-left-" $ \leftFilePath leftFile ->
    withSystemTempFile "smoke-right-" $ \rightFilePath rightFile -> do
      Text.IO.hPutStr leftFile left
      Text.IO.hPutStr rightFile right
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
              "`"
                ++ unwords (NonEmpty.toList command)
                ++ "`"
                ++ " failed with status "
                ++ show code
                ++ "."
                ++ "\n"
                ++ Text.unpack stderr

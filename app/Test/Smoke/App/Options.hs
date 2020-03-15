{-# LANGUAGE ApplicativeDo #-}

module Test.Smoke.App.Options
  ( parseOptions,
  )
where

import Data.List (intercalate)
import qualified Data.Vector as Vector
import Data.Vector (Vector)
import Options.Applicative
import Test.Smoke
  ( Args (..),
    Command (..),
    CommandLine (..),
    Mode (..),
    Options (..),
  )
import qualified Test.Smoke.App.Diff as Diff
import Test.Smoke.App.OptionTypes
import qualified Test.Smoke.App.Shell as Shell
import Test.Smoke.Paths (parseFile)

type IsTTY = Bool

parseOptions :: IO AppOptions
parseOptions = do
  isTTY <- Shell.isTTY
  foundDiffEngine <- Diff.findEngine
  execParser (options isTTY foundDiffEngine)

options :: IsTTY -> Diff.Engine -> ParserInfo AppOptions
options isTTY foundDiffEngine =
  info
    (optionParser isTTY foundDiffEngine <**> helper)
    ( fullDesc
        <> header "Smoke: a framework for testing most things from the very edges."
    )

optionParser :: IsTTY -> Diff.Engine -> Parser AppOptions
optionParser isTTY foundDiffEngine = do
  executionCommand <- commandParser
  mode <- modeParser
  color <- colorParser isTTY
  diffEngine <- diffEngineParser foundDiffEngine
  testLocation <- testLocationParser
  return
    AppOptions
      { optionsExecution =
          Options
            { optionsCommand = executionCommand,
              optionsTestLocations = testLocation
            },
        optionsColor = color,
        optionsMode = mode,
        optionsDiffEngine = diffEngine
      }

commandParser :: Parser (Maybe Command)
commandParser = do
  commandString <-
    optional $
      strOption (long "command" <> help "Specify or override the command to run")
  return $ constructCommand <$> commandString
  where
    constructCommand :: String -> Command
    constructCommand commandString =
      let (program : args) = words commandString
       in CommandArgs
            (CommandLine (parseFile program) (Args (Vector.fromList args)))

modeParser :: Parser Mode
modeParser =
  flag' Bless (long "bless" <> help "Bless the results") <|> pure Check

colorParser :: IsTTY -> Parser ColorOutput
colorParser isTTY =
  flag' Color (short 'c' <> long "color" <> help "Color output")
    <|> flag' NoColor (long "no-color" <> help "Do not color output")
    <|> pure
      ( if isTTY
          then Color
          else NoColor
      )

diffEngineParser :: Diff.Engine -> Parser Diff.Engine
diffEngineParser foundDiffEngine =
  option
    (str >>= readDiffEngine)
    (long "diff" <> help "Specify the diff engine" <> value foundDiffEngine)
  where
    readDiffEngine :: String -> ReadM Diff.Engine
    readDiffEngine =
      maybe (readerError ("Valid diff engines are: " ++ validDiffEngine)) return
        . Diff.getEngine
    validDiffEngine = intercalate ", " Diff.engineNames

testLocationParser :: Parser (Vector FilePath)
testLocationParser =
  Vector.fromList <$> some (argument str (metavar "TEST-LOCATION..."))

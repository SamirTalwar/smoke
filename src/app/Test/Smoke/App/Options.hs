{-# LANGUAGE ApplicativeDo #-}

module Test.Smoke.App.Options
  ( parseOptions,
  )
where

import Data.List (intercalate)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Options.Applicative
import Test.Smoke
  ( Args (..),
    Command (..),
    CommandLine (..),
    Mode (..),
    Options (..),
  )
import Test.Smoke.App.Diff qualified as Diff
import Test.Smoke.App.OptionTypes
import Test.Smoke.App.Shell qualified as Shell
import Test.Smoke.Paths (parseFile)

type IsTTY = Bool

parseOptions :: IO InitOptions
parseOptions = do
  isTTY <- Shell.isTTY
  foundDiffEngine <- Diff.findEngine
  execParser (options isTTY foundDiffEngine)

options :: IsTTY -> Diff.Engine -> ParserInfo InitOptions
options isTTY foundDiffEngine =
  info
    ( (optionParser isTTY foundDiffEngine <|> versionParser)
        <**> helper
    )
    ( fullDesc
        <> header "Smoke: a framework for testing most things from the very edges."
    )

versionParser :: Parser InitOptions
versionParser =
  flag' ShowVersionText (long "version" <> help "Show the version string" <> hidden)
    <|> flag' ShowVersionNumeric (long "version-numeric" <> help "Show the version number" <> hidden)

optionParser :: IsTTY -> Diff.Engine -> Parser InitOptions
optionParser isTTY foundDiffEngine = do
  executionCommand <- commandParser
  mode <- modeParser
  color <- colorParser isTTY
  diffEngine <- diffEngineParser foundDiffEngine
  testLocation <- testLocationParser
  pure $
    InitAppOptions
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
  optional $
    option
      (str >>= readCommand)
      (long "command" <> help "Specify or override the command to run")
  where
    readCommand :: String -> ReadM Command
    readCommand commandString =
      case words commandString of
        [] -> readerError "Empty command."
        (program : args) -> pure $ CommandArgs (CommandLine (parseFile program) (Args (Vector.fromList args)))

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
      maybe (readerError ("Valid diff engines are: " ++ validDiffEngine)) pure
        . Diff.getEngine
    validDiffEngine = intercalate ", " Diff.engineNames

testLocationParser :: Parser (Vector FilePath)
testLocationParser =
  Vector.fromList <$> some (argument str (metavar "TEST-LOCATION..."))

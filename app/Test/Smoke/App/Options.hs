{-# LANGUAGE ApplicativeDo #-}

module Test.Smoke.App.Options
  ( AppOptions(..)
  , parseOptions
  ) where

import Data.List (intercalate)
import Data.Semigroup ((<>))
import Options.Applicative
import Test.Smoke (Command, Options(..))
import qualified Test.Smoke.App.Diff as Diff
import qualified Test.Smoke.App.Shell as Shell

data AppOptions = AppOptions
  { optionsExecution :: Options
  , optionsColor :: Bool
  , optionsBless :: Bool
  , optionsDiffEngine :: Diff.Engine
  }

parseOptions :: IO AppOptions
parseOptions = do
  isTTY <- Shell.isTTY
  foundDiffEngine <- Diff.findEngine
  execParser (options isTTY foundDiffEngine)

options :: Bool -> Diff.Engine -> ParserInfo AppOptions
options isTTY foundDiffEngine =
  info
    (optionParser isTTY foundDiffEngine <**> helper)
    (fullDesc <>
     header "Smoke: a framework for testing most things from the very edges.")

optionParser :: Bool -> Diff.Engine -> Parser AppOptions
optionParser isTTY foundDiffEngine = do
  executionCommand <- commandParser
  bless <- blessParser
  color <- colorParser isTTY
  diffEngine <- diffEngineParser foundDiffEngine
  testLocation <- testLocationParser
  return
    AppOptions
      { optionsExecution =
          Options
            { optionsCommand = executionCommand
            , optionsTestLocations = testLocation
            }
      , optionsColor = color
      , optionsBless = bless
      , optionsDiffEngine = diffEngine
      }

commandParser :: Parser (Maybe Command)
commandParser =
  optional
    (words <$>
     strOption (long "command" <> help "Specify or override the command to run"))

blessParser :: Parser Bool
blessParser =
  flag' True (long "bless" <> help "Bless the results") <|> pure False

colorParser :: Bool -> Parser Bool
colorParser isTTY =
  flag' True (short 'c' <> long "color" <> help "Color output") <|>
  flag' False (long "no-color" <> help "Do not color output") <|>
  pure isTTY

diffEngineParser :: Diff.Engine -> Parser Diff.Engine
diffEngineParser foundDiffEngine =
  option
    (str >>= readDiffEngine)
    (long "diff" <> help "Specify the diff engine" <> value foundDiffEngine)
  where
    readDiffEngine :: String -> ReadM Diff.Engine
    readDiffEngine =
      maybe (readerError ("Valid diff engines are: " ++ validDiffEngine)) return .
      Diff.getEngine
    validDiffEngine = intercalate ", " Diff.engineNames

testLocationParser :: Parser [FilePath]
testLocationParser = some (argument str (metavar "TEST-LOCATION..."))

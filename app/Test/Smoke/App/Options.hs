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
  , optionsDiffRenderer :: Diff.Renderer
  }

parseOptions :: IO AppOptions
parseOptions = do
  isTTY <- Shell.isTTY
  foundDiffRenderer <- Diff.findRenderer
  execParser (options isTTY foundDiffRenderer)

options :: Bool -> Diff.Renderer -> ParserInfo AppOptions
options isTTY foundDiffRenderer =
  info
    (optionParser isTTY foundDiffRenderer <**> helper)
    (fullDesc <>
     header "Smoke: a framework for testing most things from the very edges.")

optionParser :: Bool -> Diff.Renderer -> Parser AppOptions
optionParser isTTY foundDiffRenderer = do
  executionCommand <- commandParser
  bless <- blessParser
  color <- colorParser isTTY
  diffRenderer <- diffRendererParser foundDiffRenderer
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
      , optionsDiffRenderer = diffRenderer
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

diffRendererParser :: Diff.Renderer -> Parser Diff.Renderer
diffRendererParser foundDiffRenderer =
  option
    (str >>= readDiffRenderer)
    (long "diff" <> help "Specify the diff renderer" <> value foundDiffRenderer)
  where
    readDiffRenderer :: String -> ReadM Diff.Renderer
    readDiffRenderer =
      maybe
        (readerError ("Valid diff renderers are: " ++ validDiffRenderers))
        return .
      Diff.getRenderer
    validDiffRenderers = intercalate ", " Diff.renderers

testLocationParser :: Parser [FilePath]
testLocationParser = some (argument str (metavar "TEST-LOCATION..."))

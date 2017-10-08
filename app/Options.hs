module Options
  ( parseOptions
  ) where

import Data.Semigroup ((<>))
import Options.Applicative
import System.Posix.IO (stdOutput)
import System.Posix.Terminal (queryTerminal)
import Test.Smoke (Command, Options(Options))

parseOptions :: IO Options
parseOptions = do
  isTTY <- queryTerminal stdOutput
  execParser (options isTTY)

options :: Bool -> ParserInfo Options
options isTTY =
  info
    (optionParser isTTY <**> helper)
    (fullDesc <>
     header "Smoke: a framework for testing most things from the very edges.")

optionParser :: Bool -> Parser Options
optionParser isTTY =
  Options <$> commandParser <*> colorParser isTTY <*> testLocationParser

commandParser :: Parser (Maybe Command)
commandParser =
  optional
    (words <$>
     strOption (long "command" <> help "Specify or override the command to run"))

colorParser :: Bool -> Parser Bool
colorParser isTTY =
  flag' True (short 'c' <> long "color" <> help "Color output") <|>
  flag' False (long "no-color" <> help "Do not color output") <|>
  pure isTTY -- TODO: Make this work on Windows.

testLocationParser :: Parser [FilePath]
testLocationParser = some (argument str (metavar "TEST-LOCATION..."))

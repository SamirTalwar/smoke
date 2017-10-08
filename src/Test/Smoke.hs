module Test.Smoke
  ( Command
  , Options(..)
  , Tests
  , Test(..)
  , discoverTests
  , runTests
  ) where

import Control.Applicative
import Control.Monad
import System.Directory
import System.FilePath
import System.FilePath.Glob
import qualified Test.Smoke.FileTypes as FileTypes

type Command = [String]

data Options = Options
  { optionsCommand :: Maybe Command
  , optionsColor :: Bool
  , optionsTestLocations :: [FilePath]
  } deriving (Eq, Show)

type Tests = [Test]

data Test = Test
  {
  } deriving (Eq, Show)

discoverTests :: Options -> IO Tests
discoverTests options =
  concat <$>
  forM
    (optionsTestLocations options)
    (\location -> do
       command <-
         return (optionsCommand options) <<|>>
         readCommandFile (location </> "command") <<|>>
         readCommandFile (takeDirectory location </> "command")
       files <-
         concat .
         zipWith
           (\fileTypeGlob paths -> zip (repeat fileTypeGlob) paths)
           (map fst FileTypes.globs) .
         fst <$>
         globDir (map snd FileTypes.globs) location
       print command
       print files
       return [])

readCommandFile :: FilePath -> IO (Maybe Command)
readCommandFile path = do
  exists <- doesFileExist path
  if exists
    then Just . lines <$> readFile path
    else return Nothing

runTests :: Tests -> IO ()
runTests tests = do
  print tests
  return ()

infixl 3 <<|>>

(<<|>>) :: IO (Maybe a) -> IO (Maybe a) -> IO (Maybe a)
(<<|>>) = liftM2 (<|>)

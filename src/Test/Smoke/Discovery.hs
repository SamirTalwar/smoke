module Test.Smoke.Discovery
  ( discoverTests
  ) where

import Control.Applicative ((<|>))
import Control.Monad (forM, liftM2, when)
import Data.Function (on)
import Data.List (find, groupBy, sortBy)
import Data.Maybe (fromJust, fromMaybe, isNothing)
import System.Directory
import System.FilePath
import System.FilePath.Glob
import Test.Smoke.FileTypes (FileType)
import qualified Test.Smoke.FileTypes as FileTypes
import Test.Smoke.Types

discoverTests :: Options -> IO Tests
discoverTests options =
  concat <$>
  forM
    (optionsTestLocations options)
    (discoverTestsInLocation (optionsCommand options))

discoverTestsInLocation :: Maybe Command -> FilePath -> IO [Test]
discoverTestsInLocation commandFromOptions location = do
  command <- findCommand
  files <- allFiles
  let grouped = groupBy ((==) `on` (dropExtension . snd)) files
  forM grouped (constructTestFromGroup location command)
  where
    findCommand =
      return commandFromOptions <<|>>
      readCommandFileIfExists (location </> "command") <<|>>
      readCommandFileIfExists (takeDirectory location </> "command")
    allFiles =
      sortBy (compare `on` snd) .
      concat .
      zipWith
        (\fileTypeGlob paths -> zip (repeat fileTypeGlob) paths)
        (map fst FileTypes.globs) .
      fst <$>
      globDir (map snd FileTypes.globs) location

constructTestFromGroup ::
     FilePath -> Maybe Command -> [(FileType, FilePath)] -> IO Test
constructTestFromGroup location commandForLocation group = do
  let part fileType = snd <$> find ((== fileType) . fst) group
  let parts fileType = snd <$> filter ((== fileType) . fst) group
  let name = makeRelative location $ dropExtension (snd (head group))
  command <-
    sequence (readCommandFile <$> part FileTypes.Command) <<|>>
    return commandForLocation
  args <- sequence (readCommandFile <$> part FileTypes.Args)
  let stdIn = part FileTypes.StdIn
  let stdOut = parts FileTypes.StdOut
  let stdErr = parts FileTypes.StdErr
  status <- fromMaybe (return 0) (readStatusFile <$> part FileTypes.Status)
  when
    (isNothing command)
    (fail ("The test \"" ++ name ++ "\" has no command."))
  when
    (isNothing args && isNothing stdIn)
    (fail ("The test \"" ++ name ++ "\" has no args or STDIN."))
  when
    (null stdOut && null stdErr)
    (fail ("The test \"" ++ name ++ "\" has no STDOUT or STDERR."))
  return
    Test
    { testName = name
    , testCommand = fromJust command
    , testArgs = args
    , testStdIn = stdIn
    , testStdOut = stdOut
    , testStdErr = stdErr
    , testStatus = status
    }

readCommandFileIfExists :: FilePath -> IO (Maybe Command)
readCommandFileIfExists path = do
  exists <- doesFileExist path
  if exists
    then Just <$> readCommandFile path
    else return Nothing

readCommandFile :: FilePath -> IO Command
readCommandFile path = lines <$> readFile path

readStatusFile :: FilePath -> IO Int
readStatusFile path = read <$> readFile path

(<<|>>) :: IO (Maybe a) -> IO (Maybe a) -> IO (Maybe a)
(<<|>>) = liftM2 (<|>)

infixl 3 <<|>>

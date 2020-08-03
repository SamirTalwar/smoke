{-# LANGUAGE OverloadedStrings #-}

module Test.Smoke.App.PrintResults
  ( printResult,
  )
where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Test.Smoke
import Test.Smoke.App.Diff
import Test.Smoke.App.OptionTypes
import Test.Smoke.App.Print
import Test.Smoke.App.PrintErrors
import Test.Smoke.Paths
import Text.Printf (printf)

printResult :: TestResult -> Output ()
printResult (TestResult _ TestSuccess) = putGreenLn "  succeeded"
printResult (TestResult test (TestFailure testPlan statusResult stdOutResult stdErrResult fileResults)) = do
  printFailingInput
    "args"
    ( Text.unlines . Vector.toList . Vector.map fromString . unArgs
        <$> testArgs test
    )
  printFailingInput "input" (unStdIn <$> (planStdIn testPlan <$ testStdIn test))
  printFailingOutput "status" ((<> "\n") . showInt . unStatus <$> statusResult)
  printFailingOutput "stdout" (unStdOut <$> stdOutResult)
  printFailingOutput "stderr" (unStdErr <$> stdErrResult)
  printFailingFilesOutput fileResults
printResult (TestResult _ (TestError testError)) = printTestError testError
printResult (TestResult _ TestIgnored) = putYellowLn "  ignored"

printFailingInput :: Foldable f => String -> f Text -> Output ()
printFailingInput name value =
  forM_ value $ \v -> do
    putRed $ fromString $ indentedKey ("  " ++ name ++ ":")
    putPlainLn $ indented outputIndentation v

printFailingOutput :: String -> PartResult Text -> Output ()
printFailingOutput _ PartSuccess = return ()
printFailingOutput name (PartFailure comparisons) = do
  putRed $ fromString $ indentedKey ("  " ++ name ++ ":")
  uncurry printFailure (Vector.head comparisons)
  forM_ (Vector.tail comparisons) $ \(expected, actual) -> do
    putRed "      or: "
    printFailure expected actual

printFailingFilesOutput ::
  Map (RelativePath File) (PartResult TestFileContents) -> Output ()
printFailingFilesOutput fileResults =
  if all isSuccess (Map.elems fileResults)
    then return ()
    else do
      putRedLn "  files:"
      forM_ (Map.assocs fileResults) $ \(path, fileResult) ->
        printFailingFileOutput path (unTestFileContents <$> fileResult)
  where
    isSuccess PartSuccess = True
    isSuccess (PartFailure _) = False

printFailingFileOutput :: RelativePath File -> PartResult Text -> Output ()
printFailingFileOutput _ PartSuccess = return ()
printFailingFileOutput path (PartFailure comparisons) = do
  putRedLn $ fromString ("    " ++ toFilePath path ++ ":")
  putPlain $ fromString $ indentedKey ""
  uncurry printFailure (Vector.head comparisons)
  forM_ (Vector.tail comparisons) $ \(expected, actual) -> do
    putRed "      or: "
    printFailure expected actual

printFailure :: Assert Text -> Text -> Output ()
printFailure (AssertEqual expected) = printDiff expected

printDiff :: Text -> Text -> Output ()
printDiff left right = do
  AppOptions
    { optionsColor = color,
      optionsDiffEngine = DiffEngine {engineRender = renderDiff}
    } <-
    ask
  diff <- liftIO $ renderDiff color left right
  putPlainLn $ indented outputIndentation diff

indentedKey :: String -> String
indentedKey = printf ("%-" ++ show outputIndentation ++ "s")

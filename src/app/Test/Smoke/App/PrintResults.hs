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

data PartName = ShortName String | LongName String

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
printFailingOutput name = printFailures (ShortName name)

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
printFailingFileOutput path = printFailures (LongName ("  " ++ toFilePath path))

printFailures :: PartName -> PartResult Text -> Output ()
printFailures _ PartSuccess =
  return ()
printFailures name (PartFailure (SingleAssertionFailure failure)) = do
  printFailureName name
  printFailure failure
printFailures name (PartFailure (MultipleAssertionFailures failures)) = do
  printFailureName name
  printFailure (Vector.head failures)
  forM_ (Vector.tail failures) $ \failure -> do
    putRed "      or: "
    printFailure failure

printFailureName :: PartName -> Output ()
printFailureName (ShortName name) =
  putRed $ fromString $ indentedKey ("  " ++ name ++ ":")
printFailureName (LongName name) = do
  putRedLn $ fromString ("  " ++ name ++ ":")
  putPlain $ fromString $ indentedKey ""

printFailure :: AssertionFailure Text -> Output ()
printFailure (AssertionFailureDiff expected actual) = printDiff expected actual

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

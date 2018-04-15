{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteStringChar
import Data.Char (ord)
import Data.Maybe (fromJust)
import Data.Word (Word8)
import Options
import System.Console.ANSI
import System.Exit
import Test.Smoke
import Text.Printf (printf)

type Output a = ReaderT AppOptions IO a

main :: IO ()
main = do
  options <- parseOptions
  tests <- discoverTests (optionsExecution options)
  results <- runTests tests
  runReaderT
    (do printResults results
        printSummary results)
    options
  exitAccordingTo results

printResults :: TestResults -> Output ()
printResults = mapM_ printResult

printResult :: TestResult -> Output ()
printResult (TestSuccess test) = do
  printTitle (testName test)
  putGreenLn "  succeeded"
printResult (TestFailure (TestExecutionPlan test _ _ stdIn) statusResult stdOutResult stdErrResult) = do
  printTitle (testName test)
  printFailingInput "args" (unlines <$> testArgs test)
  printFailingInput "input" (unStdIn <$> stdIn)
  printFailingOutput "status" (show . unStatus <$> statusResult)
  printFailingOutput "output" (unStdOut <$> stdOutResult)
  printFailingOutput "error" (unStdErr <$> stdErrResult)
printResult (TestError test NoCommandFile) = do
  printTitle (testName test)
  printError "There is no command file."
printResult (TestError test NoInputFiles) = do
  printTitle (testName test)
  printError "There are no args or STDIN files."
printResult (TestError test NoOutputFiles) = do
  printTitle (testName test)
  printError "There are no STDOUT or STDERR files."
printResult (TestError test NonExistentCommand) = do
  printTitle (testName test)
  printError $
    "The application \"" ++
    unwords (fromJust (testCommand test)) ++ "\" does not exist."
printResult (TestError test NonExecutableCommand) = do
  printTitle (testName test)
  printError $
    "The application \"" ++
    unwords (fromJust (testCommand test)) ++ "\" is not executable."
printResult (TestError test (CouldNotExecuteCommand e)) = do
  printTitle (testName test)
  printError $
    "The application \"" ++
    unwords (fromJust (testCommand test)) ++ "\" could not be executed.\n" ++ e

printTitle :: String -> Output ()
printTitle = liftIO . putStrLn

printFailingInput :: (Printable p, Foldable f) => String -> f p -> Output ()
printFailingInput name value =
  forM_ value $ \v -> do
    putRed (indentedKey ("  " ++ name ++ ":"))
    putRedLn (indented outputIndentation v)

printFailingOutput :: Printable p => String -> PartResult p -> Output ()
printFailingOutput _ PartSuccess = return ()
printFailingOutput name (PartFailure expected actual) = do
  putRed (indentedKey ("  actual " ++ name ++ ":"))
  putRedLn (indented outputIndentation actual)
  putRed (indentedKey ("  expected " ++ name ++ ":"))
  putRedLn (indented outputIndentation (head expected))
  forM_ (tail expected) $ \output -> do
    putRed "               or:  "
    putRedLn (indented outputIndentation output)

printError :: String -> Output ()
printError = putRedLn . indentedAll messageIndentation

printSummary :: TestResults -> Output ()
printSummary results = do
  printEmptyLn
  let testCount = length results
  let failureCount = length failures
  case failureCount of
    0 -> putGreenLn (show testCount ++ " tests, 0 failures")
    1 -> putRedLn (show testCount ++ " tests, 1 failure")
    n -> putRedLn (show testCount ++ " tests, " ++ show n ++ " failures")
  where
    failures = filter isFailure results

outputIndentation :: Int
outputIndentation = 20

messageIndentation :: Int
messageIndentation = 2

indentedKey :: String -> String
indentedKey = printf "%-20s"

class Printable p where
  printStr :: p -> IO ()
  printStrLn :: p -> IO ()
  indented :: Int -> p -> p
  indentedAll :: Int -> p -> p
  stripTrailingNewline :: p -> p
  isEmpty :: p -> Bool
  hasEsc :: p -> Bool

instance Printable String where
  printStr = putStr
  printStrLn = putStrLn
  indented n = unlines . indentedLines . lines
    where
      indentString = replicate n ' '
      indentedLines [] = []
      indentedLines (first:rest) = first : map (indentString ++) rest
  indentedAll n = unlines . map (indentString ++) . lines
    where
      indentString = replicate n ' '
  stripTrailingNewline string
    | string == "" = string
    | last string == '\n' = init string
    | otherwise = string
  isEmpty = (== "")
  hasEsc string = '\ESC' `elem` string

instance Printable ByteString where
  printStr = ByteStringChar.putStr
  printStrLn = ByteStringChar.putStrLn
  indented n = ByteStringChar.unlines . indentedLines . ByteStringChar.lines
    where
      indentString = ByteString.replicate n space
      indentedLines [] = []
      indentedLines (first:rest) =
        first : map (\line -> ByteString.concat [indentString, line]) rest
  indentedAll n =
    ByteStringChar.unlines .
    map (\line -> ByteString.concat [indentString, line]) . ByteStringChar.lines
    where
      indentString = ByteString.replicate n (fromIntegral $ ord ' ')
  stripTrailingNewline string
    | string == ByteString.empty = string
    | ByteString.last string == newline = ByteString.init string
    | otherwise = string
  isEmpty string = ByteString.length string == 0
  hasEsc string = esc `ByteString.elem` string

space :: Word8
space = fromIntegral $ ord ' '

newline :: Word8
newline = fromIntegral $ ord '\n'

esc :: Word8
esc = fromIntegral $ ord '\ESC'

printEmptyLn :: Output ()
printEmptyLn = liftIO $ putStrLn ""

putGreen :: Printable p => p -> Output ()
putGreen = putColor Green

putGreenLn :: Printable p => p -> Output ()
putGreenLn = putColorLn Green

putRed :: Printable p => p -> Output ()
putRed = putColor Red

putRedLn :: Printable p => p -> Output ()
putRedLn = putColorLn Red

putColor :: Printable p => Color -> p -> Output ()
putColor color string = do
  options <- ask
  if optionsColor options && not (hasEsc string)
    then do
      liftIO $ setSGR [SetColor Foreground Dull color]
      liftIO $ printStr string
      liftIO $ setSGR [Reset]
    else liftIO $ printStr string

putColorLn :: Printable p => Color -> p -> Output ()
putColorLn color string = do
  putColor color (stripTrailingNewline string)
  printEmptyLn

exitAccordingTo :: TestResults -> IO ()
exitAccordingTo results =
  if failureCount == 0
    then exitSuccess
    else exitWith (ExitFailure 1)
  where
    failureCount = length (filter isFailure results)

isFailure :: TestResult -> Bool
isFailure TestSuccess {} = False
isFailure TestFailure {} = True
isFailure TestError {} = True

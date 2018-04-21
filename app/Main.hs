{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import Control.Exception (displayException)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Algorithm.Diff (Diff(..), getGroupedDiff)
import Data.Algorithm.DiffOutput (DiffOperation(..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteStringChar
import Data.Char (ord)
import Data.Maybe (fromJust)
import Data.String (IsString(..))
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
  if optionsBless options
    then outputResults options =<< blessResults results
    else outputResults options results

outputResults :: AppOptions -> TestResults -> IO ()
outputResults options results = do
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
printResult (TestError test (BlessingFailed e)) = do
  printTitle (testName test)
  printError $ "Blessing failed.\n" ++ displayException e
printResult (TestError test CouldNotBlessStdOutWithMultipleValues) = do
  printTitle (testName test)
  printError
    "There are multiple expected STDOUT values, so the result cannot be blessed.\n"
printResult (TestError test CouldNotBlessStdErrWithMultipleValues) = do
  printTitle (testName test)
  printError
    "There are multiple expected STDERR values, so the result cannot be blessed.\n"

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
  putRed (indentedKey ("  " ++ name ++ ":"))
  putDiff (head expected) actual
  forM_ (tail expected) $ \e -> do
    putRed "      or: "
    putDiff e actual
  where
    putDiff :: Printable p => p -> p -> Output ()
    putDiff left right =
      putRedLn $
      indented outputIndentation $
      prettyPrintDiff $ getGroupedDiff (lines' left) (lines' right)

data LineRange p =
  LineRange (Int, Int)
            [p]
  deriving (Show, Read, Eq, Ord)

prettyPrintDiff :: Printable p => [Diff [p]] -> p
prettyPrintDiff = mconcat . map prettyPrintOperation . diffToLineRanges
  where
    diffToLineRanges :: [Diff [p]] -> [DiffOperation (LineRange p)]
    diffToLineRanges = toLineRange 1 1
      where
        toLineRange :: Int -> Int -> [Diff [p]] -> [DiffOperation (LineRange p)]
        toLineRange _ _ [] = []
        toLineRange leftLine rightLine (Both ls _:rs) =
          let lins = length ls
           in toLineRange (leftLine + lins) (rightLine + lins) rs
        toLineRange leftLine rightLine (Second lsS:First lsF:rs) =
          toChange leftLine rightLine lsF lsS rs
        toLineRange leftLine rightLine (First lsF:Second lsS:rs) =
          toChange leftLine rightLine lsF lsS rs
        toLineRange leftLine rightLine (Second lsS:rs) =
          let linesS = length lsS
              diff =
                Addition
                  (LineRange (rightLine, rightLine + linesS - 1) lsS)
                  (leftLine - 1)
           in diff : toLineRange leftLine (rightLine + linesS) rs
        toLineRange leftLine rightLine (First lsF:rs) =
          let linesF = length lsF
              diff =
                Deletion
                  (LineRange (leftLine, leftLine + linesF - 1) lsF)
                  (rightLine - 1)
           in diff : toLineRange (leftLine + linesF) rightLine rs
        toChange leftLine rightLine lsF lsS rs =
          let linesS = length lsS
              linesF = length lsF
           in Change
                (LineRange (leftLine, leftLine + linesF - 1) lsF)
                (LineRange (rightLine, rightLine + linesS - 1) lsS) :
              toLineRange (leftLine + linesF) (rightLine + linesS) rs
    prettyPrintOperation :: Printable p => DiffOperation (LineRange p) -> p
    prettyPrintOperation (Deletion (LineRange leftNumbers leftContents) lineNoRight) =
      mconcat
        [ prettyRange leftNumbers
        , fromString "d"
        , int lineNoRight
        , fromString "\n"
        , prettyLines '<' leftContents
        ]
    prettyPrintOperation (Addition (LineRange rightNumbers rightContents) lineNoLeft) =
      mconcat
        [ int lineNoLeft
        , fromString "a"
        , prettyRange rightNumbers
        , fromString "\n"
        , prettyLines '>' rightContents
        ]
    prettyPrintOperation (Change (LineRange leftNumbers leftContents) (LineRange rightNumbers rightContents)) =
      mconcat
        [ prettyRange leftNumbers
        , fromString "c"
        , prettyRange rightNumbers
        , fromString "\n"
        , prettyLines '<' leftContents
        , fromString "---\n"
        , prettyLines '>' rightContents
        ]
    prettyRange :: Printable p => (Int, Int) -> p
    prettyRange (start, end) =
      if start == end
        then int start
        else mconcat [int start, fromString ",", int end]
    prettyLines :: Printable p => Char -> [p] -> p
    prettyLines start = unlines' . map (mappend (fromString [start, ' ']))

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
outputIndentation = 10

messageIndentation :: Int
messageIndentation = 2

indentedKey :: String -> String
indentedKey = printf ("%-" ++ show outputIndentation ++ "s")

class (Eq p, Ord p, Monoid p, IsString p) =>
      Printable p
  where
  int :: Int -> p
  spaces :: Int -> p
  lines' :: p -> [p]
  unlines' :: [p] -> p
  indented :: Int -> p -> p
  indented n = unlines' . indentedLines . lines'
    where
      indentedLines :: Printable p => [p] -> [p]
      indentedLines [] = []
      indentedLines (first:rest) = first : map (mappend (spaces n)) rest
  indentedAll :: Int -> p -> p
  indentedAll n = unlines' . map (mappend (spaces n)) . lines'
  stripTrailingNewline :: p -> p
  isEmpty :: p -> Bool
  hasEsc :: p -> Bool
  printStr :: p -> IO ()
  printStrLn :: p -> IO ()

instance Printable String where
  int = show
  spaces n = replicate n ' '
  lines' = lines
  unlines' = unlines
  stripTrailingNewline string
    | string == "" = string
    | last string == '\n' = init string
    | otherwise = string
  isEmpty = (== "")
  hasEsc string = '\ESC' `elem` string
  printStr = putStr
  printStrLn = putStrLn

instance Printable ByteString where
  int = fromString . show
  spaces n = ByteString.replicate n space
  lines' = ByteStringChar.lines
  unlines' = ByteStringChar.unlines
  stripTrailingNewline string
    | string == ByteString.empty = string
    | ByteString.last string == newline = ByteString.init string
    | otherwise = string
  isEmpty string = ByteString.length string == 0
  hasEsc string = esc `ByteString.elem` string
  printStr = ByteStringChar.putStr
  printStrLn = ByteStringChar.putStrLn

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

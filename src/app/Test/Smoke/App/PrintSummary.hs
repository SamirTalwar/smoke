{-# LANGUAGE OverloadedStrings #-}

module Test.Smoke.App.PrintSummary
  ( printSummary,
  )
where

import Data.Text (Text)
import Test.Smoke
import Test.Smoke.App.Print

printSummary :: Summary -> Output ()
printSummary summary = do
  putEmptyLn
  let testCount = summaryTotal summary
  let failureCount = summaryFailures summary
  let ignoredCount = summaryIgnored summary
  let testWord = pluralize testCount "test" "tests"
  let failureWord = pluralize failureCount "failure" "failures"
  let printSummaryLine =
        if failureCount == 0
          then putGreenLn
          else putRedLn
  printSummaryLine $
    showInt testCount
      <> " "
      <> testWord
      <> ", "
      <> showInt failureCount
      <> " "
      <> failureWord
      <> (if ignoredCount > 0 then ", " <> showInt ignoredCount <> " ignored" else "")

pluralize :: Int -> Text -> Text -> Text
pluralize 1 singular _ = singular
pluralize _ _ plural = plural

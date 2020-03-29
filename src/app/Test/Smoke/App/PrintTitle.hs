module Test.Smoke.App.PrintTitle
  ( printTitle,
  )
where

import Control.Monad.IO.Class (liftIO)
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Test.Smoke
import Test.Smoke.App.OptionTypes
import Test.Smoke.App.Print

printTitle :: ShowSuiteNames -> SuiteName -> Maybe TestName -> Output ()
printTitle showSuiteNames thisSuiteName thisTestName = liftIO $ putStrLn name
  where
    suiteNameForPrinting =
      if showSuiteNames || Maybe.isNothing thisTestName
        then Just thisSuiteName
        else Nothing
    name =
      List.intercalate "/" $
        Maybe.catMaybes
          [unSuiteName <$> suiteNameForPrinting, unTestName <$> thisTestName]

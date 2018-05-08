module Test.Smoke.App.Diff
  ( DiffEngine(..)
  , Engine
  , engines
  , engineNames
  , findEngine
  , getEngine
  ) where

import Control.Monad (filterM)
import Data.List (find)
import qualified Test.Smoke.App.Diff.DiffUtility as DiffUtility
import qualified Test.Smoke.App.Diff.Native as Native
import Test.Smoke.App.Diff.Types

type Engine = DiffEngine

engines :: [DiffEngine]
engines = [DiffUtility.engine, Native.engine]

engineNames :: [String]
engineNames = map engineName engines

findEngine :: IO DiffEngine
findEngine = head <$> filterM engineEnabled engines

getEngine :: String -> Maybe DiffEngine
getEngine name = find (\engine -> name == engineName engine) engines

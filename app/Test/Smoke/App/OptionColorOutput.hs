module Test.Smoke.App.OptionColorOutput
  ( ColorOutput (..),
  )
where

data ColorOutput
  = Color
  | NoColor
  deriving (Eq)

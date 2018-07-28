{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Test.Smoke.App.Print where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask)
import qualified Data.Maybe as Maybe
import Data.String (IsString(..))
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import System.Console.ANSI
import Test.Smoke (Contents)
import Test.Smoke.App.OptionTypes (AppOptions(..), ColorOutput(..))

type Output a = ReaderT AppOptions IO a

int :: Int -> Contents
int = fromString . show

hasEsc :: Contents -> Bool
hasEsc = Maybe.isJust . Text.find (== '\ESC')

spaces :: Int -> Contents
spaces n = Text.replicate n space

space :: Contents
space = " "

newline :: Contents
newline = "\n"

indented :: Int -> Contents -> Contents
indented n = Text.unlines . indented' . Text.lines
  where
    indented' [] = []
    indented' (first:rest) = first : map (mappend (spaces n)) rest

indentedAll :: Int -> Contents -> Contents
indentedAll n = Text.unlines . map (mappend (spaces n)) . Text.lines

putEmptyLn :: Output ()
putEmptyLn = liftIO $ putStrLn ""

putPlainLn :: Contents -> Output ()
putPlainLn contents = do
  liftIO $ TextIO.putStr contents
  unless (newline `Text.isSuffixOf` contents) putEmptyLn

putGreenLn :: Contents -> Output ()
putGreenLn = putColorLn Green

putRed :: Contents -> Output ()
putRed = putColor Red

putRedLn :: Contents -> Output ()
putRedLn = putColorLn Red

putColor :: Color -> Contents -> Output ()
putColor color contents = do
  options <- ask
  if optionsColor options == Color && not (hasEsc contents)
    then do
      liftIO $ setSGR [SetColor Foreground Dull color]
      liftIO $ TextIO.putStr contents
      liftIO $ setSGR [Reset]
    else liftIO $ TextIO.putStr contents

putColorLn :: Color -> Contents -> Output ()
putColorLn color contents = do
  putColor color contents
  unless (newline `Text.isSuffixOf` contents) putEmptyLn

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Test.Smoke.App.Print where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask)
import qualified Data.Maybe as Maybe
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import System.Console.ANSI
import System.IO (stderr)
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

putColorLn :: Color -> Contents -> Output ()
putColorLn color contents = do
  putColor color contents
  unless (newline `Text.isSuffixOf` contents) putEmptyLn

putColor :: Color -> Contents -> Output ()
putColor = putColorWith TextIO.putStr

putColorWith :: (Text -> IO ()) -> Color -> Contents -> Output ()
putColorWith put color contents = do
  options <- ask
  liftIO $
    if optionsColor options == Color && not (hasEsc contents)
      then do
        setSGR [SetColor Foreground Dull color]
        put contents
        setSGR [Reset]
      else put contents

putError :: Contents -> Output ()
putError contents = do
  putColorWith (TextIO.hPutStr stderr) Red contents
  unless (newline `Text.isSuffixOf` contents) $
    liftIO $ TextIO.hPutStrLn stderr ""

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Test.Smoke.App.Print where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteStringChar
import Data.Char (ord)
import Data.String (IsString(..))
import Data.Word (Word8)
import System.Console.ANSI
import Test.Smoke.App.OptionTypes (AppOptions(..), ColorOutput(..))

type OutputString = ByteString

type Output a = ReaderT AppOptions IO a

int :: Int -> OutputString
int = fromString . show

isEmpty :: OutputString -> Bool
isEmpty string = ByteString.length string == 0

hasEsc :: OutputString -> Bool
hasEsc string = esc `ByteString.elem` string

spaces :: Int -> OutputString
spaces n = ByteString.replicate n space

space :: Word8
space = fromIntegral $ ord ' '

newline :: Word8
newline = fromIntegral $ ord '\n'

esc :: Word8
esc = fromIntegral $ ord '\ESC'

indented :: Int -> OutputString -> OutputString
indented n = ByteStringChar.unlines . indentedLines . ByteStringChar.lines
  where
    indentedLines :: [OutputString] -> [OutputString]
    indentedLines [] = []
    indentedLines (first:rest) = first : map (mappend (spaces n)) rest

indentedAll :: Int -> OutputString -> OutputString
indentedAll n =
  ByteStringChar.unlines . map (mappend (spaces n)) . ByteStringChar.lines

stripTrailingNewline :: OutputString -> OutputString
stripTrailingNewline string
  | string == ByteString.empty = string
  | ByteString.last string == newline = ByteString.init string
  | otherwise = string

putEmptyLn :: Output ()
putEmptyLn = liftIO $ putStrLn ""

putPlainLn :: OutputString -> Output ()
putPlainLn string = do
  liftIO $ ByteStringChar.putStr $ stripTrailingNewline string
  putEmptyLn

putGreen :: OutputString -> Output ()
putGreen = putColor Green

putGreenLn :: OutputString -> Output ()
putGreenLn = putColorLn Green

putRed :: OutputString -> Output ()
putRed = putColor Red

putRedLn :: OutputString -> Output ()
putRedLn = putColorLn Red

putColor :: Color -> OutputString -> Output ()
putColor color string = do
  options <- ask
  if optionsColor options == Color && not (hasEsc string)
    then do
      liftIO $ setSGR [SetColor Foreground Dull color]
      liftIO $ ByteStringChar.putStr string
      liftIO $ setSGR [Reset]
    else liftIO $ ByteStringChar.putStr string

putColorLn :: Color -> OutputString -> Output ()
putColorLn color string = do
  putColor color (stripTrailingNewline string)
  putEmptyLn

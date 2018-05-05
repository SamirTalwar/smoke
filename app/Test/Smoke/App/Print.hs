{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Test.Smoke.App.Print where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteStringChar
import Data.Char (ord)
import Data.String (IsString(..))
import Data.Word (Word8)

type OutputString = ByteString

int :: Int -> OutputString
int = fromString . show

printStr :: OutputString -> IO ()
printStr = ByteStringChar.putStr

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

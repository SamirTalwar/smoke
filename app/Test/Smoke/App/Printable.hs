{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Test.Smoke.App.Printable
  ( Printable(..)
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteStringChar
import Data.Char (ord)
import Data.String (IsString(..))
import Data.Word (Word8)

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

space :: Word8
space = fromIntegral $ ord ' '

newline :: Word8
newline = fromIntegral $ ord '\n'

esc :: Word8
esc = fromIntegral $ ord '\ESC'

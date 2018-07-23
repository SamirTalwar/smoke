module Test.Smoke.Lines
  ( normalizeLines
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteStringChar
import Data.Char (ord)
import Data.Word (Word8)

normalizeLines :: ByteString -> [ByteString]
normalizeLines = map removeCarriageReturn . ByteStringChar.lines

removeCarriageReturn :: ByteString -> ByteString
removeCarriageReturn s
  | s == ByteString.empty = s
  | ByteString.last s == carriageReturn = ByteString.init s
  | otherwise = s

carriageReturn :: Word8
carriageReturn = fromIntegral $ ord '\r'

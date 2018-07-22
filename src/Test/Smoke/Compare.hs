module Test.Smoke.Compare
  ( compareByteStrings
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteStringChar
import Data.Word (Word8)

compareByteStrings :: ByteString -> ByteString -> Bool
compareByteStrings a b = normalizeLines a == normalizeLines b

normalizeLines :: ByteString -> [ByteString]
normalizeLines = map removeCarriageReturn . ByteStringChar.lines

removeCarriageReturn :: ByteString -> ByteString
removeCarriageReturn s
  | s == ByteString.empty = s
  | ByteString.last s == carriageReturn = ByteString.init s
  | otherwise = s

carriageReturn :: Word8
carriageReturn = 13

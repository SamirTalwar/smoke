module Test.Smoke.Fixtures
  ( FixtureContents(..)
  ) where

import Control.Exception (throwIO)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Test.Smoke.Types

class FixtureContents a where
  readFixture :: Fixture a -> IO a
  writeFixture :: Fixture a -> a -> IO ()

instance FixtureContents Status where
  readFixture = readFixture' (Status . read . Text.unpack)
  writeFixture = writeFixture' "exit-status" (Text.pack . show . unStatus)

instance FixtureContents StdIn where
  readFixture = readFixture' StdIn
  writeFixture = writeFixture' "stdin" unStdIn

instance FixtureContents StdOut where
  readFixture = readFixture' StdOut
  writeFixture = writeFixture' "stdout" unStdOut

instance FixtureContents StdErr where
  readFixture = readFixture' StdErr
  writeFixture = writeFixture' "stderr" unStdErr

readFixture' :: (Contents -> a) -> Fixture a -> IO a
readFixture' _ (InlineFixture fixture) = return fixture
readFixture' construct (FileFixture path) = construct <$> TextIO.readFile path

writeFixture' :: String -> (a -> Contents) -> Fixture a -> a -> IO ()
writeFixture' name deconstruct (InlineFixture value) _ =
  throwIO $ CouldNotWriteFixture name (deconstruct value)
writeFixture' _ deconstruct (FileFixture path) value =
  TextIO.writeFile path (deconstruct value)

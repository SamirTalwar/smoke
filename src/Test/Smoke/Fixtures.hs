module Test.Smoke.Fixtures
  ( FixtureContents(..)
  , readFixture
  , writeFixture
  ) where

import Control.Exception (throwIO)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Test.Smoke.Types

class FixtureContents a where
  fixtureName :: a -> String
  serializeFixture :: a -> Contents
  deserializeFixture :: Contents -> a

instance FixtureContents Status where
  fixtureName = const "exit-status"
  serializeFixture = Text.pack . show . unStatus
  deserializeFixture = Status . read . Text.unpack

instance FixtureContents StdIn where
  fixtureName = const "stdin"
  serializeFixture = unStdIn
  deserializeFixture = StdIn

instance FixtureContents StdOut where
  fixtureName = const "stdout"
  serializeFixture = unStdOut
  deserializeFixture = StdOut

instance FixtureContents StdErr where
  fixtureName = const "stderr"
  serializeFixture = unStdErr
  deserializeFixture = StdErr

readFixture :: FixtureContents a => Fixture a -> IO a
readFixture (InlineFixture fixture) = return fixture
readFixture (FileFixture path) = deserializeFixture <$> TextIO.readFile path

writeFixture :: FixtureContents a => Fixture a -> a -> IO ()
writeFixture (InlineFixture value) _ =
  throwIO $ CouldNotWriteFixture (fixtureName value) (serializeFixture value)
writeFixture (FileFixture path) value =
  TextIO.writeFile path (serializeFixture value)

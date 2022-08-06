module Test.Smoke.Spec.PathsSpec where

import Control.Monad.IO.Class (liftIO)
import Hedgehog
import qualified Hedgehog.Range as Range
import qualified System.Directory as Directory
import Test.Hspec
import Test.Hspec.Hedgehog
import Test.Smoke.Paths
import Test.Smoke.Spec.PathGenerator

spec :: Spec
spec =
  describe "path processing" $ do
    it "normalizes" $
      hedgehog $ do
        filePath <- forAll $ genRelativeFilePath (Range.linear 1 10)
        let path = parseFile filePath
        let rePath = parseFile $ toFilePath path
        path === rePath

    it "resolves a relative path" $
      hedgehog $ do
        filePath <- forAll $ genRelativeFilePath (Range.linear 1 10)
        absoluteFilePath <- liftIO $ Directory.makeAbsolute filePath
        let expected = parseFile absoluteFilePath
        let path = parseFile filePath
        actual <- liftIO $ resolve path
        toFilePath actual === toFilePath expected

    it "resolves an absolute path" $
      hedgehog $ do
        filePath <- forAll $ genAbsoluteFilePath (Range.linear 1 10)
        let path = parseFile filePath
        actual <- liftIO $ resolve path
        let expected = path
        toFilePath actual === toFilePath expected

    it "finds the parent of a path" $
      hedgehog $ do
        parentDir <- forAll $ genRelativeDir (Range.linear 1 10)
        resolvedParentDir <- liftIO $ resolve parentDir
        child <- parseFile <$> forAll genNamedSegment
        let path = resolvedParentDir </> child
        let actual = parent path
        let expected = resolvedParentDir
        actual === expected

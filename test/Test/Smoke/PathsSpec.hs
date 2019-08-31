{-# LANGUAGE FlexibleContexts #-}

module Test.Smoke.PathsSpec where

import Control.Monad.IO.Class (liftIO)
import qualified Data.List as List
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import Test.Hspec
import Test.Smoke.Paths

spec :: Spec
spec =
  parallel $
  describe "path processing" $ do
    it "normalizes" $
      require $
      property $ do
        filePath <- forAll $ genRelativeFilePath (Range.linear 1 10)
        let path = parseFile filePath
        let rePath = parseFile $ toFilePath path
        path === rePath
    it "resolves a relative path" $
      require $
      property $ do
        filePath <- forAll $ genRelativeFilePath (Range.linear 1 10)
        let path = parseFile filePath
        actual <- liftIO $ resolve path
        workingDirectory <- liftIO Directory.getCurrentDirectory
        let expected = parseFile $ workingDirectory FilePath.</> filePath
        toFilePath actual === toFilePath expected
    it "resolves an absolute path" $
      require $
      property $ do
        filePath <- forAll $ genAbsoluteFilePath (Range.linear 1 10)
        let path = parseFile filePath
        actual <- liftIO $ resolve path
        let expected = path
        toFilePath actual === toFilePath expected
    it "finds the parent of a path" $
      require $
      property $ do
        parentDir <- forAll $ genRelativeDir (Range.linear 1 10)
        resolvedParentDir <- liftIO $ resolve parentDir
        child <- forAll $ genNotCurrentDir $ genRelativeFile (Range.singleton 1)
        let path = resolvedParentDir </> child
        let actual = parent path
        let expected = resolvedParentDir
        actual === expected

genRelativeDir :: Range Int -> Gen (RelativePath Dir)
genRelativeDir segmentRange = parseDir <$> genRelativeFilePath segmentRange

genRelativeFile :: Range Int -> Gen (RelativePath File)
genRelativeFile segmentRange = parseFile <$> genRelativeFilePath segmentRange

genRelativeFilePath :: Range Int -> Gen FilePath
genRelativeFilePath segmentRange = do
  segments <-
    Gen.list segmentRange (Gen.string (Range.linear 0 100) Gen.alphaNum)
  let joined = List.intercalate [FilePath.pathSeparator] segments
  return $ List.dropWhile (== FilePath.pathSeparator) joined

genAbsoluteFilePath :: Range Int -> Gen FilePath
genAbsoluteFilePath segmentRange = ("/" ++) <$> genRelativeFilePath segmentRange

genNotCurrentDir :: Path p t => Gen (p t) -> Gen (p t)
genNotCurrentDir =
  Gen.filter
    (flip
       notElem
       ("." : map (\separator -> ['.', separator]) FilePath.pathSeparators) .
     toFilePath)

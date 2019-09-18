{-# LANGUAGE CPP #-}
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
        absoluteFilePath <- liftIO $ Directory.makeAbsolute filePath
        let expected = parseFile absoluteFilePath
        let path = parseFile filePath
        actual <- liftIO $ resolve path
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
        child <- parseFile <$> forAll genNamedSegment
        let path = resolvedParentDir </> child
        let actual = parent path
        let expected = resolvedParentDir
        actual === expected

data FilePathSegment
  = Current
  | Parent
  | Named String

segmentString :: FilePathSegment -> String
segmentString Current = "."
segmentString Parent = ".."
segmentString (Named value) = value

genNamedSegment :: Gen FilePath
genNamedSegment = Gen.string (Range.linear 1 100) Gen.alphaNum

genSegment :: Gen FilePathSegment
genSegment =
  Gen.frequency
    [ (2, Gen.constant Current)
    , (1, Gen.constant Parent)
    , (5, Named <$> genNamedSegment)
    ]

segmentsAreAdditive :: [FilePathSegment] -> Bool
segmentsAreAdditive = (> 0) . countSegments

segmentsAreNotSubtractive :: [FilePathSegment] -> Bool
segmentsAreNotSubtractive = (>= 0) . countSegments

countSegment :: FilePathSegment -> Int
countSegment Current = 0
countSegment Parent = -1
countSegment (Named _) = 1

countSegments :: [FilePathSegment] -> Int
countSegments = sum . map countSegment

genRelativeDir :: Range Int -> Gen (RelativePath Dir)
genRelativeDir segmentRange = parseDir <$> genRelativeFilePath segmentRange

genRelativeFile :: Range Int -> Gen (RelativePath File)
genRelativeFile segmentRange = parseFile <$> genRelativeFilePath segmentRange

genRelativeFilePath :: Range Int -> Gen FilePath
genRelativeFilePath segmentRange = do
  segments <-
    Gen.filter segmentsAreNotSubtractive $ Gen.list segmentRange genSegment
  let joined =
        List.intercalate [FilePath.pathSeparator] $ map segmentString segments
  return $ dropWhile (== FilePath.pathSeparator) joined

genAbsoluteFilePath :: Range Int -> Gen FilePath
genAbsoluteFilePath segmentRange =
  FilePath.joinDrive rootDirectory <$> genRelativeFilePath segmentRange

rootDirectory :: FilePath
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
rootDirectory = "C:\\"
#else
rootDirectory = "/"
#endif

module Test.Smoke.Spec.PathGenerator
  ( genAbsoluteFilePath,
    genRelativeDir,
    genRelativeFile,
    genRelativeFilePath,
    genNamedSegment,
  )
where

import Data.List qualified as List
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.FilePath qualified as FilePath
import Test.Smoke.Paths
import Test.Smoke.Spec.RootDirectory

data FilePathSegment
  = Current
  | Parent
  | Named String

genRelativeDir :: Range Int -> Gen (Path Relative Dir)
genRelativeDir segmentRange = parseDir <$> genRelativeFilePath segmentRange

genRelativeFile :: Range Int -> Gen (Path Relative File)
genRelativeFile segmentRange = parseFile <$> genRelativeFilePath segmentRange

genAbsoluteFilePath :: Range Int -> Gen FilePath
genAbsoluteFilePath segmentRange =
  FilePath.joinDrive rootDirectory <$> genRelativeFilePath segmentRange

genRelativeFilePath :: Range Int -> Gen FilePath
genRelativeFilePath segmentRange = do
  segments <-
    Gen.filter segmentsAreNotSubtractive $ Gen.list segmentRange genSegment
  let joined =
        List.intercalate [FilePath.pathSeparator] $ map segmentString segments
  pure $ dropWhile (== FilePath.pathSeparator) joined

genSegment :: Gen FilePathSegment
genSegment =
  Gen.frequency
    [ (2, Gen.constant Current),
      (1, Gen.constant Parent),
      (5, Named <$> genNamedSegment)
    ]

genNamedSegment :: Gen FilePath
genNamedSegment = do
  name <- Gen.string (Range.linear 1 100) Gen.alphaNum
  trailingSeparators <-
    Gen.string (Range.linear 0 3) $ Gen.constant FilePath.pathSeparator
  pure $ name <> trailingSeparators

segmentsAreNotSubtractive :: [FilePathSegment] -> Bool
segmentsAreNotSubtractive = (>= 0) . countSegments

countSegment :: FilePathSegment -> Int
countSegment Current = 0
countSegment Parent = -1
countSegment (Named _) = 1

countSegments :: [FilePathSegment] -> Int
countSegments = sum . map countSegment

segmentString :: FilePathSegment -> String
segmentString Current = "."
segmentString Parent = ".."
segmentString (Named value) = value

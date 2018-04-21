module Test.Smoke.App.Diff
  ( prettyPrintDiff
  ) where

import Data.Algorithm.Diff (Diff(..))
import Data.Algorithm.DiffOutput (DiffOperation(..))
import Data.String (fromString)
import Test.Smoke.App.Printable

data LineRange p =
  LineRange (Int, Int)
            [p]
  deriving (Show, Read, Eq, Ord)

prettyPrintDiff :: Printable p => [Diff [p]] -> p
prettyPrintDiff = mconcat . map prettyPrintOperation . diffToLineRanges
  where
    diffToLineRanges :: [Diff [p]] -> [DiffOperation (LineRange p)]
    diffToLineRanges = toLineRange 1 1
      where
        toLineRange :: Int -> Int -> [Diff [p]] -> [DiffOperation (LineRange p)]
        toLineRange _ _ [] = []
        toLineRange leftLine rightLine (Both ls _:rs) =
          let lins = length ls
           in toLineRange (leftLine + lins) (rightLine + lins) rs
        toLineRange leftLine rightLine (Second lsS:First lsF:rs) =
          toChange leftLine rightLine lsF lsS rs
        toLineRange leftLine rightLine (First lsF:Second lsS:rs) =
          toChange leftLine rightLine lsF lsS rs
        toLineRange leftLine rightLine (Second lsS:rs) =
          let linesS = length lsS
              diff =
                Addition
                  (LineRange (rightLine, rightLine + linesS - 1) lsS)
                  (leftLine - 1)
           in diff : toLineRange leftLine (rightLine + linesS) rs
        toLineRange leftLine rightLine (First lsF:rs) =
          let linesF = length lsF
              diff =
                Deletion
                  (LineRange (leftLine, leftLine + linesF - 1) lsF)
                  (rightLine - 1)
           in diff : toLineRange (leftLine + linesF) rightLine rs
        toChange leftLine rightLine lsF lsS rs =
          let linesS = length lsS
              linesF = length lsF
           in Change
                (LineRange (leftLine, leftLine + linesF - 1) lsF)
                (LineRange (rightLine, rightLine + linesS - 1) lsS) :
              toLineRange (leftLine + linesF) (rightLine + linesS) rs
    prettyPrintOperation :: Printable p => DiffOperation (LineRange p) -> p
    prettyPrintOperation (Deletion (LineRange leftNumbers leftContents) lineNoRight) =
      mconcat
        [ prettyRange leftNumbers
        , fromString "d"
        , int lineNoRight
        , fromString "\n"
        , prettyLines '<' leftContents
        ]
    prettyPrintOperation (Addition (LineRange rightNumbers rightContents) lineNoLeft) =
      mconcat
        [ int lineNoLeft
        , fromString "a"
        , prettyRange rightNumbers
        , fromString "\n"
        , prettyLines '>' rightContents
        ]
    prettyPrintOperation (Change (LineRange leftNumbers leftContents) (LineRange rightNumbers rightContents)) =
      mconcat
        [ prettyRange leftNumbers
        , fromString "c"
        , prettyRange rightNumbers
        , fromString "\n"
        , prettyLines '<' leftContents
        , fromString "---\n"
        , prettyLines '>' rightContents
        ]
    prettyRange :: Printable p => (Int, Int) -> p
    prettyRange (start, end) =
      if start == end
        then int start
        else mconcat [int start, fromString ",", int end]
    prettyLines :: Printable p => Char -> [p] -> p
    prettyLines start = unlines' . map (mappend (fromString [start, ' ']))

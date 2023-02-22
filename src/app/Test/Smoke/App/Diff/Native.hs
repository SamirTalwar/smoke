module Test.Smoke.App.Diff.Native
  ( engine,
  )
where

import Data.Algorithm.Diff (Diff, PolyDiff (..), getGroupedDiff)
import Data.Algorithm.DiffOutput (DiffOperation (..))
import Data.String (fromString)
import Data.Text qualified as Text
import Test.Smoke.App.Diff.Types

data LineRange
  = LineRange (Int, Int) [Text]
  deriving (Eq, Ord)

engine :: DiffEngine
engine =
  DiffEngine {engineName = name, engineEnabled = enabled, engineRender = render}

name :: String
name = "native"

enabled :: IO Bool
enabled = pure True

render :: PrintDiff
render _ left right =
  pure $
    mconcat $
      map prettyPrintOperation $
        diffToLineRanges $
          getGroupedDiff (Text.lines left) (Text.lines right)
  where
    diffToLineRanges :: [Diff [Text]] -> [DiffOperation LineRange]
    diffToLineRanges = toLineRange 1 1
      where
        toLineRange :: Int -> Int -> [Diff [Text]] -> [DiffOperation LineRange]
        toLineRange _ _ [] = []
        toLineRange leftLine rightLine (Both ls _ : rs) =
          let lins = length ls
           in toLineRange (leftLine + lins) (rightLine + lins) rs
        toLineRange leftLine rightLine (Second lsS : First lsF : rs) =
          toChange leftLine rightLine lsF lsS rs
        toLineRange leftLine rightLine (First lsF : Second lsS : rs) =
          toChange leftLine rightLine lsF lsS rs
        toLineRange leftLine rightLine (Second lsS : rs) =
          let linesS = length lsS
              diff =
                Addition
                  (LineRange (rightLine, rightLine + linesS - 1) lsS)
                  (leftLine - 1)
           in diff : toLineRange leftLine (rightLine + linesS) rs
        toLineRange leftLine rightLine (First lsF : rs) =
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
                (LineRange (rightLine, rightLine + linesS - 1) lsS)
                : toLineRange (leftLine + linesF) (rightLine + linesS) rs
    prettyPrintOperation :: DiffOperation LineRange -> Text
    prettyPrintOperation (Deletion (LineRange leftNumbers leftContents) lineNoRight) =
      mconcat
        [ prettyRange leftNumbers,
          fromString "d",
          fromString $ show lineNoRight,
          fromString "\n",
          prettyLines '<' leftContents
        ]
    prettyPrintOperation (Addition (LineRange rightNumbers rightContents) lineNoLeft) =
      mconcat
        [ fromString $ show lineNoLeft,
          fromString "a",
          prettyRange rightNumbers,
          fromString "\n",
          prettyLines '>' rightContents
        ]
    prettyPrintOperation (Change (LineRange leftNumbers leftContents) (LineRange rightNumbers rightContents)) =
      mconcat
        [ prettyRange leftNumbers,
          fromString "c",
          prettyRange rightNumbers,
          fromString "\n",
          prettyLines '<' leftContents,
          fromString "---\n",
          prettyLines '>' rightContents
        ]
    prettyRange :: (Int, Int) -> Text
    prettyRange (start, end) =
      if start == end
        then fromString (show start)
        else
          mconcat
            [fromString (show start), fromString ",", fromString (show end)]
    prettyLines :: Char -> [Text] -> Text
    prettyLines start = Text.unlines . map (mappend (fromString [start, ' ']))

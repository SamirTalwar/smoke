module Test.Smoke.App.Diff.Native
  ( engine
  ) where

import Data.Algorithm.Diff (Diff(..), getGroupedDiff)
import Data.Algorithm.DiffOutput (DiffOperation(..))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteStringChar
import Data.String (fromString)
import Test.Smoke.App.Diff.Types

type OutputString = ByteString

data LineRange =
  LineRange (Int, Int)
            [OutputString]
  deriving (Eq, Ord)

engine :: DiffEngine
engine = DiffEngine {engineName = name, engineRender = render}

name :: String
name = "native"

render :: RenderDiff
render left right =
  return $
  mconcat $
  map prettyPrintOperation $
  diffToLineRanges $
  getGroupedDiff (ByteStringChar.lines left) (ByteStringChar.lines right)
  where
    diffToLineRanges :: [Diff [OutputString]] -> [DiffOperation LineRange]
    diffToLineRanges = toLineRange 1 1
      where
        toLineRange ::
             Int -> Int -> [Diff [OutputString]] -> [DiffOperation LineRange]
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
    prettyPrintOperation :: DiffOperation LineRange -> OutputString
    prettyPrintOperation (Deletion (LineRange leftNumbers leftContents) lineNoRight) =
      mconcat
        [ prettyRange leftNumbers
        , fromString "d"
        , fromString $ show lineNoRight
        , fromString "\n"
        , prettyLines '<' leftContents
        ]
    prettyPrintOperation (Addition (LineRange rightNumbers rightContents) lineNoLeft) =
      mconcat
        [ fromString $ show lineNoLeft
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
    prettyRange :: (Int, Int) -> OutputString
    prettyRange (start, end) =
      if start == end
        then fromString (show start)
        else mconcat
               [fromString (show start), fromString ",", fromString (show end)]
    prettyLines :: Char -> [OutputString] -> OutputString
    prettyLines start =
      ByteStringChar.unlines . map (mappend (fromString [start, ' ']))

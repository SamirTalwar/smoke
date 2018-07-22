{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Test.Smoke.App.Print where

import Control.Monad (forM_, mapM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteStringChar
import Data.Char (ord)
import Data.String (IsString(..))
import Data.Word (Word8)
import System.Console.ANSI
import Test.Smoke.App.OptionTypes (AppOptions(..), ColorOutput(..))

type OutputString = [OutputLine]

type OutputLine = ByteString

type Output a = ReaderT AppOptions IO a

single :: OutputLine -> OutputString
single = return

serialize :: OutputString -> ByteString
serialize = ByteStringChar.unlines

deserialize :: ByteString -> OutputString
deserialize = ByteStringChar.lines

int :: Int -> OutputLine
int = fromString . show

hasEsc :: OutputLine -> Bool
hasEsc = ByteString.elem esc

spaces :: Int -> OutputLine
spaces n = ByteString.replicate n space

space :: Word8
space = fromIntegral $ ord ' '

newline :: Word8
newline = fromIntegral $ ord '\n'

esc :: Word8
esc = fromIntegral $ ord '\ESC'

indented :: Int -> OutputString -> OutputString
indented _ [] = []
indented n (first:rest) = first : map (mappend (spaces n)) rest

indentedAll :: Int -> OutputString -> OutputString
indentedAll n = map (mappend (spaces n))

putEmptyLn :: Output ()
putEmptyLn = liftIO $ putStrLn ""

putPlainLn :: OutputString -> Output ()
putPlainLn string = liftIO $ mapM_ ByteStringChar.putStrLn string

putGreenLn :: OutputString -> Output ()
putGreenLn = putColorLn Green

putRed :: OutputLine -> Output ()
putRed = putColor Red

putRedLn :: OutputString -> Output ()
putRedLn = putColorLn Red

putColor :: Color -> OutputLine -> Output ()
putColor color string = do
  options <- ask
  if optionsColor options == Color && not (hasEsc string)
    then do
      liftIO $ setSGR [SetColor Foreground Dull color]
      liftIO $ ByteStringChar.putStr string
      liftIO $ setSGR [Reset]
    else liftIO $ ByteStringChar.putStr string

putColorLn :: Color -> OutputString -> Output ()
putColorLn color string =
  forM_ string $ \line -> do
    putColor color line
    putEmptyLn

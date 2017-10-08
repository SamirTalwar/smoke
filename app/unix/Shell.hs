module Shell where

import System.Posix.IO (stdOutput)
import System.Posix.Terminal (queryTerminal)

isTTY :: IO Bool
isTTY = queryTerminal stdOutput

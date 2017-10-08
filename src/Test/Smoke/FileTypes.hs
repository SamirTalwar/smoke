module Test.Smoke.FileTypes
  ( FileType(..)
  , glob
  , globs
  ) where

import qualified System.FilePath.Glob as Glob

data FileType
  = Command
  | Args
  | StdIn
  | StdOut
  | StdErr
  | Status
  deriving (Eq, Show)

allFileTypes :: [FileType]
allFileTypes = [Command, Args, StdIn, StdOut, StdErr, Status]

glob :: FileType -> Glob.Pattern
glob Command = Glob.compile "*.command"
glob Args = Glob.compile "*.args"
glob StdIn = Glob.compile "*.in"
glob StdOut = Glob.compile "*.out*"
glob StdErr = Glob.compile "*.err*"
glob Status = Glob.compile "*.status"

globs :: [(FileType, Glob.Pattern)]
globs = zip allFileTypes $ map glob allFileTypes

module Free.Coproduct where

import Prelude (class Functor, Unit, pure, bind, id, unit, (<<<), (*>), ($))
import Node.Path (FilePath)
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Node.FS.Sync (readTextFile, writeTextFile)
import Data.Either (Either(..))
import Data.Functor.Coproduct (Coproduct(..), left, right)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Free (Free, foldFree, liftF)

-- | This demonstrates a way of composing multiple free monad DSLs together
data FileSystemF next
  = WriteFile Encoding FilePath String next
  | ReadFile Encoding FilePath (String -> next)

type FileSystem = Free FileSystemF

data ConsoleF next
  = ConsoleLog String next
  | ConsoleError String next

type Console = Free ConsoleF


instance functorFileSystemF :: Functor FileSystemF where
  map f (WriteFile encoding filePath string next) = WriteFile encoding filePath string (f next)
  map f (ReadFile encoding filePath fNext) = ReadFile encoding filePath (f <<< fNext)

instance functorConsoleF :: Functor ConsoleF where
  map f (ConsoleLog string next) = ConsoleLog string (f next)
  map f (ConsoleError string next) = ConsoleError string (f next)


-- | We can compose two free monad DSLs with Coproduct, which composes two functors
type AppF = Coproduct FileSystemF ConsoleF
type App = Free AppF


-- | Some helper functions
liftFileSystem :: forall a. FileSystemF a -> App a
liftFileSystem = liftF <<< left

liftConsole :: forall a. ConsoleF a -> App a
liftConsole = liftF <<< right


-- | Helper functions for FileSystem
writeFile ::  Encoding -> FilePath -> String -> App Unit
writeFile encoding filePath string = liftFileSystem $ WriteFile encoding filePath string unit

readFile :: Encoding -> FilePath -> App String
readFile encoding filePath = liftFileSystem $ ReadFile encoding filePath id


-- | Helper functions for Console
consoleLog :: String -> App Unit
consoleLog string = liftConsole $ ConsoleLog string unit

consoleError :: String -> App Unit
consoleError string = liftConsole $ ConsoleError string unit


-- | The FileSystem interpreter
interpretFs :: forall a e. FileSystemF a -> Eff (fs :: FS, exception :: EXCEPTION | e) a
interpretFs (WriteFile encoding filePath string next) = do
  _ <- writeTextFile encoding filePath string
  pure next
interpretFs (ReadFile encoding filePath fNext) = do
  contents <- readTextFile encoding filePath
  pure (fNext contents)

-- | The Console interpreter
interpretConsole :: forall a e. ConsoleF a -> Eff (console :: CONSOLE | e) a
interpretConsole (ConsoleLog string next) = log string *> pure next
interpretConsole (ConsoleError string next) = log string *> pure next

-- | The App interpreter that composes Console and FileSystem
interpretApp :: forall a e. AppF a -> Eff (fs :: FS, console :: CONSOLE, exception :: EXCEPTION | e) a
interpretApp x = case x of
  Coproduct (Left l) -> interpretFs l
  Coproduct (Right r) -> interpretConsole r


-- | This program just reads foo.txt and prints its contents to stdout
catFoo :: App Unit
catFoo = do
  contents <- readFile UTF8 "foo.txt"
  consoleLog contents


main :: forall e. Eff (fs :: FS, console :: CONSOLE, exception :: EXCEPTION | e) Unit
main = foldFree interpretApp catFoo

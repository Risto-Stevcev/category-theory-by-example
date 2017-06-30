module Free.FileSystem where

import Prelude (class Functor, Unit, pure, bind, id, unit, (<<<), (<>), (*>), ($))
import Node.Path (FilePath)
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Node.FS.Sync (readTextFile, writeTextFile)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Free (Free, foldFree, liftF)

-- | Here's another example of a free monad DSL
-- | Notice the pattern in free monads: it converts one abstract syntax tree into another. The DSL is
-- | parameterized by `next` because we'll need to eventually provide an interpreter from `FileSystemF a -> Eff e a`,
-- | or more generally a natural transformation from `f ~> g`, or in this case `FileSystemF ~> Eff e`.
-- |
-- | The `String -> next` looks a little unusual, since we know that readFileText will return a `String`,
-- | But remember that it can't return a concrete type like String because the interpreter needs to be a natural
-- | transformation (`f ~> g`), so we have to keep the AST general (using `next`). And it gives the free monad the
-- | chance to pass the returned value to the next command using `bind`
data FileSystemF next
  = WriteFile Encoding FilePath String next
  | ReadFile Encoding FilePath (String -> next)

type FileSystem = Free FileSystemF

instance functorFileSystemF :: Functor FileSystemF where
  map f (WriteFile encoding filePath string next) = WriteFile encoding filePath string (f next)
  -- | The `f <<< fNext` lets the free monad pass the value using `bind`
  -- | ie, the `contents` in: `readFile UTF8 "foo.txt" >>= \contents`
  map f (ReadFile encoding filePath fNext) = ReadFile encoding filePath (f <<< fNext)

-- | These just provide some nicer functions for working with free DSLs
writeFile :: Encoding -> FilePath -> String -> FileSystem Unit
writeFile encoding filePath string = liftF $ WriteFile encoding filePath string unit

readFile :: Encoding -> FilePath -> FileSystem String
readFile encoding filePath = liftF $ ReadFile encoding filePath id


-- | Here's a sample program
-- | It just saves "hello, world!" to `foo.txt`, reads it, and then saves an extra "!" at the end
echoHello :: FileSystem Unit
echoHello = do
  _ <- writeFile UTF8 "foo.txt" "hello, world!"
  contents <- readFile UTF8 "foo.txt"
  writeFile UTF8 "foo.txt" (contents <> "!")

interpreter :: forall a e. FileSystemF a -> Eff (fs :: FS, exception :: EXCEPTION | e) a
interpreter x = case x of
  WriteFile encoding filePath string next -> writeTextFile encoding filePath string *> pure next
  ReadFile encoding filePath fNext -> do
    contents <- readTextFile encoding filePath
    -- | Remember the odd `String -> next` in the AST that was defined? this is why we needed it. The interpreter can
    -- | pass the `contents` with `bind`
    pure (fNext contents)

main :: forall e. Eff (fs :: FS, exception :: EXCEPTION | e) Unit
main = foldFree interpreter echoHello

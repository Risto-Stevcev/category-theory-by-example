module Free.Monad where

import Prelude (class Functor, Unit, pure, bind, id, unit, (<<<))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Free (Free, foldFree, liftF)

-- | Free monad provides a very flexible way to write a DSL. You can write a language that you can write multiple 
-- | different interpreters for. It's more powerful than free applicatives because as a monad it's capable of 
-- | interpreting dynamic data at runtime. All IO/Eff can be rewritten using free monads.

-- | If you are trying to write a data structure that you want to interpret in more than one way, and that relies on 
-- | information provided at runtime, then you're probably looking for a free monad. Another use case is if you want 
-- | to build up a monadic context that you want to unwrap and interpret at a later time. An example of this would be 
-- | for a unit testing framework.

-- | The following example is a canonical and simple example of how it works. This was taken from one of Tel's lectures
-- | and it describes how to write a simple Teletype that echoes what your print.

-- | This is the abstract syntax tree. The syntax accepts two different commands: PutLine and GetLine.
data TeletypeF a
  = PutLine String a
  | GetLine (String -> a)

-- | This is a Teletype synonym to be the Free version of the data structure.
type Teletype = Free TeletypeF

-- | In order to get a free monad, the data structure needs to be a functor first.
instance functorTeletypeF :: Functor TeletypeF where
  map f (PutLine s a) = PutLine s (f a)
  map f (GetLine f2) = GetLine (f <<< f2)

-- | Free versions of putLineTT and getLineTT.
putLineTT :: String -> Teletype Unit
putLineTT line = liftF (PutLine line unit)

getLineTT :: Teletype String
getLineTT = liftF (GetLine id)

-- | This echoTT function just echoes what the user inputs in the REPL.
echoTT :: Teletype Unit
echoTT = do
  line <- getLineTT
  case line of
    "quit" -> putLineTT "Bye!"  -- typing "quit" exits the REPL
    _ -> do
      putLineTT line
      echoTT

-- | This is the actual interpreter. It takes the TeletypeF data structure and converts it to the type you want to use.
-- | In this case the interpreter translates the TeletypeF DSL to the Eff monad.
-- | This shows the power and potential of the free monad. You can write many flavors of interpreters, and it can be 
-- | used for mocking anything monadic such as database libs, AJAX, dom manipulations, IO, etc.
interpreter :: forall a e. TeletypeF a -> Eff e a
interpreter x = case x of
  PutLine line a -> putLine line
  GetLine next -> do
    line <- getLine
    pure (next line)

-- | This is the command that runs the interpreter. The foldFree function takes the interpreter/translator and the 
-- | free monad, and it will unravel the free monad and translate it to Eff.
echoEff :: forall e. Eff e Unit
echoEff = foldFree interpreter echoTT

foreign import getLine :: forall e. Eff ( | e ) String
foreign import putLine :: forall a e. String -> Eff ( | e ) a

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = echoEff 

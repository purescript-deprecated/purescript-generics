module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.Generic (class Generic, GenericSignature(..), GenericSpine(..), fromSpine, gCompare, gEq, gShow, toSignature, toSpine, withSignature)
import Data.Maybe (Maybe(..))
import Test.Assert (ASSERT, assert')
import Type.Proxy (Proxy(..))

data Foo
  = Foo Number String
  | Bar Number
  | Quux (Array String)
  | Baz {a :: Maybe String, bq :: Number} String
  | Corge (Array Char)

derive instance genericFoo :: Generic Foo

data IntList
  = IntList Number IntList
  | NilIntList

derive instance genericIntList :: Generic IntList

data UnitPlus = UnitPlus Unit Unit

derive instance genericUnitPlus :: Generic UnitPlus

data VoidPlus = VoidPlus Void Void

derive instance genericVoidPlus :: Generic VoidPlus

instance showFoo :: Show Foo where
  show = gShow

instance eqFoo :: Eq Foo where
  eq = gEq

instance ordFoo :: Ord Foo where
  compare = gCompare

instance showIntList :: Show IntList where
  show = gShow

newtype MyNewString = MyNewString String

derive instance genericNew :: Generic MyNewString

instance showNewInt :: Show MyNewString where
  show = gShow

toFrom :: forall a. Generic a => a -> Maybe a
toFrom x = fromSpine (toSpine x)

data Bar = Bar1 Int
derive instance genericBar :: Generic Bar

main :: Eff (console :: CONSOLE, assert :: ASSERT) Unit
main = do
  logShow $ toFrom [
    Foo 12.0 "Hello"
  , Quux ["Hi","Dere"]
  , Baz {a : Just "yo", bq : 22.0} "oy"
  , Corge ['H', 'i', ' ', 'D', 'e', 'r', 'e'] ]
  logShow $ toSpine [
    Foo 12.0 "Hello"
  , Quux ["Hi","Dere"]
  , Baz {a : Just "yo", bq : 22.0} "oy"
  , Corge ['H', 'i', ' ', 'D', 'e', 'r', 'e'] ]
  logShow $ gCompare (Bar 12.3) (Foo 34.1 "hello")
  logShow $ gEq
    [ Foo 12.0 "Hello"
    , Quux ["Hi","Dere"]
    , Baz {a : Just "yo", bq : 22.0} "oy"
    , Corge ['H', 'i', ' ', 'D', 'e', 'r', 'e'] ]
    [ Foo 12.0 "Hello"
    , Quux ["Hi","Dere"]
    , Baz {a : Just "yo", bq : 22.0} "yo"
    , Corge ['H', 'i', ' ', 'D', 'e', 'r', 'e'] ]

  log "Testing Show GenericSignature instance:"
  logShow (toSignature (Proxy :: Proxy Number))
  logShow (toSignature (Proxy :: Proxy Char))
  logShow (toSignature (Proxy :: Proxy (Array (Array Number))))
  logShow (toSignature (Proxy :: Proxy Foo))
  logShow (toSignature (Proxy :: Proxy (Array Foo)))
  logShow (toSignature (Proxy :: Proxy MyNewString))
  logShow (toSignature (Proxy :: Proxy Ordering))
  log "Basic test of Eq GenericSignature instance .."
  assert' "Foo == Foo:" $ toSignature (Proxy :: Proxy Foo) == toSignature (Proxy :: Proxy Foo)
  assert' "Foo /= Bar:" $ toSignature (Proxy :: Proxy Foo) /= toSignature (Proxy :: Proxy Bar)
  assert' "Foo /= Int:" $ toSignature (Proxy :: Proxy Foo) /= toSignature (Proxy :: Proxy Int)
  assert' "Foo /= Array Int:" $ toSignature (Proxy :: Proxy Foo) /= toSignature (Proxy :: Proxy (Array Int))
  assert' "Foo /= Ordering:" $ toSignature (Proxy :: Proxy Foo) /= toSignature (Proxy :: Proxy Ordering)
  withSignature (SigArray \_ -> SigInt) genericProgram
  where
    genericProgram :: forall a. Generic a => Proxy a -> Eff (console :: CONSOLE, assert :: ASSERT) Unit
    genericProgram pr = do
      -- We don't have a way to do anything useful here, since we have no way to
      -- actually construct a value of our type. With something like foreign-generic,
      -- we might be able to decode JSON to generate a value, or we might write
      -- a generic function to create a default empty value.
      -- For now, let's try to create something from an explicit spine:
      let spine = SArray [\_ -> SInt 1, \_ -> SInt 2, \_ -> SInt 3]
      case fromSpine spine of
        Nothing -> assert' "Bad spine" false
        Just (a :: a) -> log (gShow a)

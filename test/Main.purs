module Test.Main where

import Prelude
import Data.Maybe
import Data.Generic
import Data.Array
import Control.Monad.Eff.Console
import Data.Either
import Test.Assert (assert', ASSERT)
import Type.Proxy


data Foo = Foo Number String | Bar Number | Quux (Array String) | Baz {a :: Maybe String, bq :: Number} String
         | Corge (Array Char)

data IntList = IntList Number IntList | NilIntList

derive instance genericFoo :: Generic Foo

derive instance genericIntList :: Generic IntList

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

toFrom :: forall a. (Generic a) => a -> Maybe a
toFrom x = fromSpine (toSpine x)

data Bar = Bar1 Int
derive instance genericBar :: Generic Bar

main = do
  print $ show $ toFrom [
    Foo 12.0 "Hello"
  , Quux ["Hi","Dere"]
  , Baz {a : Just "yo", bq : 22.0} "oy"
  , Corge ['H', 'i', ' ', 'D', 'e', 'r', 'e'] ]
  print $ show $ gCompare (Bar 12.3) (Foo 34.1 "hello")
  print $ show $ gEq
    [ Foo 12.0 "Hello"
    , Quux ["Hi","Dere"]
    , Baz {a : Just "yo", bq : 22.0} "oy"
    , Corge ['H', 'i', ' ', 'D', 'e', 'r', 'e'] ]
    [ Foo 12.0 "Hello"
    , Quux ["Hi","Dere"]
    , Baz {a : Just "yo", bq : 22.0} "yo"
    , Corge ['H', 'i', ' ', 'D', 'e', 'r', 'e'] ]

  log "Testing Show GenericSignature instance:"
  print (toSignature (Proxy :: Proxy Number))
  print (toSignature (Proxy :: Proxy Char))
  print (toSignature (Proxy :: Proxy (Array (Array Number))))
  print (toSignature (Proxy :: Proxy Foo))
  print (toSignature (Proxy :: Proxy (Array Foo)))
  print (toSignature (Proxy :: Proxy MyNewString))
  log "Basic test of Eq GenericSignature instance .."
  assert' "Foo == Foo:" $ toSignature (Proxy :: Proxy Foo) == toSignature (Proxy :: Proxy Foo)
  assert' "Foo == Bar:" $ toSignature (Proxy :: Proxy Foo) /= toSignature (Proxy :: Proxy Bar)
  assert' "Foo == Int:" $ toSignature (Proxy :: Proxy Foo) /= toSignature (Proxy :: Proxy Int)
  assert' "Foo == Array Int:" $ toSignature (Proxy :: Proxy Foo) /= toSignature (Proxy :: Proxy (Array Int))

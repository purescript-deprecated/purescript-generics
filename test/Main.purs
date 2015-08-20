module Test.Main where

import Prelude
import Data.Maybe
import Data.Generic
import Data.Array
import Control.Monad.Eff.Console
import Data.Either

import Test.UnitTests

data Foo = Foo Number String | Bar Number | Quux (Array String) | Baz {a :: Maybe String, bq :: Number} String

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

main = do
  unitTests
  print $ show $ toFrom [Foo 12.0 "Hello", Quux ["Hi","Dere"], Baz {a : Just "yo", bq : 22.0} "oy"]
  print $ show $ gCompare (Bar 12.3) (Foo 34.1 "hello")
  print $ show $ gEq [Foo 12.0 "Hello", Quux ["Hi","Dere"], Baz {a : Just "yo", bq : 22.0} "oy"] [Foo 12.0 "Hello", Quux ["Hi","Dere"], Baz {a : Just "yo", bq : 22.0} "yo"]

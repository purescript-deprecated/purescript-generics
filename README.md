# Module Documentation

## Module Data.Generics

#### `GenericSpine`

``` purescript
data GenericSpine
  = SProd String (Array (Unit -> GenericSpine))
  | SRecord (Array { recLabel :: String, recValue :: Unit -> GenericSpine })
  | SNumber Number
  | SInt Int
  | SString String
  | SArray (Array (Unit -> GenericSpine))
```

A GenericSpine is a universal represntation of an arbitrary data structure (that does not contain function arrows).

##### Instances
``` purescript
instance eqGeneric :: Eq GenericSpine
instance ordGeneric :: Ord GenericSpine
```

#### `GenericSignature`

``` purescript
data GenericSignature
  = SigProd (Array { sigConstructor :: String, sigValues :: Array (Unit -> GenericSignature) })
  | SigRecord (Array { recLabel :: String, recValue :: Unit -> GenericSignature })
  | SigNumber
  | SigInt
  | SigString
  | SigArray (Unit -> GenericSignature)
```

A GenericSignature is a universal representation of the structure of an arbitrary data structure (that does not contain function arrows).

#### `Proxy`

``` purescript
data Proxy a
  = Proxy
```

A proxy is a simple placeholder data type to allow us to pass type-level data at runtime.

#### `anyProxy`

``` purescript
anyProxy :: forall a. Proxy a
```

#### `Generic`

``` purescript
class Generic a where
  toSpine :: a -> GenericSpine
  toSignature :: Proxy a -> GenericSignature
  fromSpine :: GenericSpine -> Maybe a
```

The Generic typeclass provides methods for sending data to/from spine representations, as well as querying about the signatures of spine representations.
For standard data structures, you can simply write "instance Generic Foo" in the module they are declared, and the instance methods will be filled in for you.

##### Instances
``` purescript
instance genericNumber :: Generic Number
instance genericInt :: Generic Int
instance genericString :: Generic String
instance genericBool :: Generic Boolean
instance genericArray :: (Generic a) => Generic (Array a)
instance genericTuple :: (Generic a, Generic b) => Generic (Tuple a b)
instance genericMaybe :: (Generic a) => Generic (Maybe a)
instance genericEither :: (Generic a, Generic b) => Generic (Either a b)
```

#### `gShow`

``` purescript
gShow :: forall a. (Generic a) => a -> String
```

This function can be used as the default instance for Show for any instnace of Generic

#### `gEq`

``` purescript
gEq :: forall a. (Generic a) => a -> a -> Boolean
```

This function can be used as the default instance for Eq for any instance of Generic

#### `gCompare`

``` purescript
gCompare :: forall a. (Generic a) => a -> a -> Ordering
```

This function can be used as the default instance for the compare method of Ord for any instance of Generic

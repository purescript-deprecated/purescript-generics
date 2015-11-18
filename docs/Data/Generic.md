## Module Data.Generic

#### `GenericSpine`

``` purescript
data GenericSpine
  = SProd String (Array (Unit -> GenericSpine))
  | SRecord (Array { recLabel :: String, recValue :: Unit -> GenericSpine })
  | SNumber Number
  | SBoolean Boolean
  | SInt Int
  | SString String
  | SChar Char
  | SArray (Array (Unit -> GenericSpine))
```

A GenericSpine is a universal represntation of an arbitrary data structure (that does not contain function arrows).

##### Instances
``` purescript
Eq GenericSpine
Ord GenericSpine
```

#### `DataConstructor`

``` purescript
type DataConstructor = { sigConstructor :: String, sigValues :: Array (Unit -> GenericSignature) }
```

Identifies a data constructor.

#### `GenericSignature`

``` purescript
data GenericSignature
  = SigProd String (Array DataConstructor)
  | SigRecord (Array { recLabel :: String, recValue :: Unit -> GenericSignature })
  | SigNumber
  | SigBoolean
  | SigInt
  | SigString
  | SigChar
  | SigArray (Unit -> GenericSignature)
```

A GenericSignature is a universal representation of the structure of an arbitrary data structure (that does not contain function arrows).

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
Generic Number
Generic Int
Generic String
Generic Char
Generic Boolean
(Generic a) => Generic (Array a)
(Generic a, Generic b) => Generic (Tuple a b)
(Generic a) => Generic (Maybe a)
(Generic a, Generic b) => Generic (Either a b)
```

#### `isValidSpine`

``` purescript
isValidSpine :: GenericSignature -> GenericSpine -> Boolean
```

Checks that the spine follows the structure defined by the signature

#### `gShow`

``` purescript
gShow :: forall a. (Generic a) => a -> String
```

This function can be used as the default instance for Show for any instance of Generic

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



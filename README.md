# Module Documentation

## Module Data.Generics

#### `Ty`

``` purescript
data Ty
  = TyNum 
  | TyStr 
  | TyBool 
  | TyArr Ty
  | TyObj [{ value :: Ty, key :: String }]
  | TyCon { args :: [Ty], tyCon :: String }
```


#### `showTy`

``` purescript
instance showTy :: Show Ty
```


#### `eqTy`

``` purescript
instance eqTy :: Eq Ty
```


#### `Tm`

``` purescript
data Tm
  = TmNum Number
  | TmStr String
  | TmBool Boolean
  | TmArr [Tm]
  | TmObj [{ value :: Tm, key :: String }]
  | TmCon { values :: [Tm], con :: String }
```


#### `showTm`

``` purescript
instance showTm :: Show Tm
```


#### `eqTm`

``` purescript
instance eqTm :: Eq Tm
```


#### `eqObjEntry`

``` purescript
eqObjEntry :: forall k v. (Eq k, Eq v) => { value :: v, key :: k } -> { value :: v, key :: k } -> Boolean
```


#### `Proxy`

``` purescript
data Proxy a
  = Proxy 
```


#### `Generic`

``` purescript
class Generic a where
  typeOf :: Proxy a -> Ty
  term :: a -> Tm
  unTerm :: Tm -> Maybe a
```


#### `genericNumber`

``` purescript
instance genericNumber :: Generic Number
```


#### `genericString`

``` purescript
instance genericString :: Generic String
```


#### `genericBoolean`

``` purescript
instance genericBoolean :: Generic Boolean
```


#### `elementProxy`

``` purescript
elementProxy :: forall a. Proxy [a] -> Proxy a
```


#### `genericArray`

``` purescript
instance genericArray :: (Generic a) => Generic [a]
```


#### `fstProxy`

``` purescript
fstProxy :: forall a b. Proxy (Tuple a b) -> Proxy a
```


#### `sndProxy`

``` purescript
sndProxy :: forall a b. Proxy (Tuple a b) -> Proxy b
```


#### `genericTuple`

``` purescript
instance genericTuple :: (Generic a, Generic b) => Generic (Tuple a b)
```


#### `maybeProxy`

``` purescript
maybeProxy :: forall a. Proxy (Maybe a) -> Proxy a
```


#### `genericMaybe`

``` purescript
instance genericMaybe :: (Generic a) => Generic (Maybe a)
```


#### `leftProxy`

``` purescript
leftProxy :: forall a b. Proxy (Either a b) -> Proxy a
```


#### `rightProxy`

``` purescript
rightProxy :: forall a b. Proxy (Either a b) -> Proxy b
```


#### `genericEither`

``` purescript
instance genericEither :: (Generic a, Generic b) => Generic (Either a b)
```


#### `sizeOf`

``` purescript
sizeOf :: Tm -> Number
```

#### `gsize`

``` purescript
gsize :: forall a. (Generic a) => a -> Number
```


#### `gshow`

``` purescript
gshow :: forall a. (Generic a) => a -> String
```

#### `geq`

``` purescript
geq :: forall a. (Generic a) => a -> a -> Boolean
```

#### `cast`

``` purescript
cast :: forall a b. (Generic a, Generic b) => a -> Maybe b
```

#### `GenericT`

``` purescript
data GenericT
  = GenericT (Tm -> Tm)
```

#### `runGenericT`

``` purescript
runGenericT :: GenericT -> Tm -> Tm
```


#### `mkT`

``` purescript
mkT :: forall a. (Generic a) => (a -> a) -> GenericT
```


#### `gmapTImpl`

``` purescript
gmapTImpl :: GenericT -> Tm -> Tm
```


#### `gmapT`

``` purescript
gmapT :: forall a. (Generic a) => GenericT -> a -> a
```


#### `everywhereImpl`

``` purescript
everywhereImpl :: GenericT -> Tm -> Tm
```


#### `everywhere`

``` purescript
everywhere :: forall a. (Generic a) => GenericT -> a -> a
```


#### `GenericQ`

``` purescript
data GenericQ r
  = GenericQ (Tm -> r)
```

#### `runGenericQ`

``` purescript
runGenericQ :: forall r. GenericQ r -> Tm -> r
```


#### `mkQ`

``` purescript
mkQ :: forall a r. (Generic a) => r -> (a -> r) -> GenericQ r
```


#### `everythingImpl`

``` purescript
everythingImpl :: forall a r. (r -> r -> r) -> GenericQ r -> Tm -> r
```


#### `everything`

``` purescript
everything :: forall a r. (Generic a) => (r -> r -> r) -> GenericQ r -> a -> r
```
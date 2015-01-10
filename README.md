# Module Documentation

## Module Data.Generics

### Types

    data GenericQ r where
      GenericQ :: (Tm -> r) -> GenericQ r

    data GenericT where
      GenericT :: (Tm -> Tm) -> GenericT

    data Proxy a where
      Proxy :: Proxy a

    data Tm where
      TmNum :: Number -> Tm
      TmStr :: String -> Tm
      TmBool :: Boolean -> Tm
      TmArr :: [Tm] -> Tm
      TmObj :: [{ value :: Tm, key :: String }] -> Tm
      TmCon :: { values :: [Tm], con :: String } -> Tm

    data Ty where
      TyNum :: Ty
      TyStr :: Ty
      TyBool :: Ty
      TyArr :: Ty -> Ty
      TyObj :: [{ value :: Ty, key :: String }] -> Ty
      TyCon :: { args :: [Ty], tyCon :: String } -> Ty


### Type Classes

    class Generic a where
      typeOf :: Proxy a -> Ty
      term :: a -> Tm
      unTerm :: Tm -> Maybe a


### Type Class Instances

    instance eqTm :: Eq Tm

    instance eqTy :: Eq Ty

    instance genericArray :: (Generic a) => Generic [a]

    instance genericBoolean :: Generic Boolean

    instance genericEither :: (Generic a, Generic b) => Generic (Either a b)

    instance genericMaybe :: (Generic a) => Generic (Maybe a)

    instance genericNumber :: Generic Number

    instance genericString :: Generic String

    instance genericTuple :: (Generic a, Generic b) => Generic (Tuple a b)

    instance showTm :: Show Tm

    instance showTy :: Show Ty


### Values

    cast :: forall a b. (Generic a, Generic b) => a -> Maybe b

    elementProxy :: forall a. Proxy [a] -> Proxy a

    eqObjEntry :: forall k v. (Eq k, Eq v) => { value :: v, key :: k } -> { value :: v, key :: k } -> Boolean

    everything :: forall a r. (Generic a) => (r -> r -> r) -> GenericQ r -> a -> r

    everythingImpl :: forall a r. (r -> r -> r) -> GenericQ r -> Tm -> r

    everywhere :: forall a. (Generic a) => GenericT -> a -> a

    everywhereImpl :: GenericT -> Tm -> Tm

    fstProxy :: forall a b. Proxy (Tuple a b) -> Proxy a

    geq :: forall a. (Generic a) => a -> a -> Boolean

    gmapT :: forall a. (Generic a) => GenericT -> a -> a

    gmapTImpl :: GenericT -> Tm -> Tm

    gshow :: forall a. (Generic a) => a -> String

    gsize :: forall a. (Generic a) => a -> Number

    leftProxy :: forall a b. Proxy (Either a b) -> Proxy a

    maybeProxy :: forall a. Proxy (Maybe a) -> Proxy a

    mkQ :: forall a r. (Generic a) => r -> (a -> r) -> GenericQ r

    mkT :: forall a. (Generic a) => (a -> a) -> GenericT

    rightProxy :: forall a b. Proxy (Either a b) -> Proxy b

    runGenericQ :: forall r. GenericQ r -> Tm -> r

    runGenericT :: GenericT -> Tm -> Tm

    sizeOf :: Tm -> Number

    sndProxy :: forall a b. Proxy (Tuple a b) -> Proxy b
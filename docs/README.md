# Module Documentation
## Module Data.Generics

### Types

    data GenericQ r where
      GenericQ :: Tm -> r -> GenericQ r

    data GenericT  where
      GenericT :: Tm -> Tm -> GenericT 

    data Proxy a where
      Proxy :: Proxy a

    data Tm  where
      TmNum :: Prim.Number -> Tm 
      TmStr :: Prim.String -> Tm 
      TmBool :: Prim.Boolean -> Tm 
      TmArr :: [Tm] -> Tm 
      TmObj :: [{ value :: Tm, key :: Prim.String }] -> Tm 
      TmCon :: { values :: [Tm], con :: Prim.String } -> Tm 

    data Ty  where
      TyNum :: Ty 
      TyStr :: Ty 
      TyBool :: Ty 
      TyArr :: Ty -> Ty 
      TyObj :: [{ value :: Ty, key :: Prim.String }] -> Ty 
      TyCon :: { args :: [Ty], tyCon :: Prim.String } -> Ty 


### Type Classes

    class Generic a where
      typeOf :: Proxy a -> Ty
      term :: a -> Tm
      unTerm :: Tm -> Maybe a


### Type Class Instances

    (Generic ((a))) => instance genericArray :: Generic ([a])

    instance genericBoolean :: Generic (Prim.Boolean)

    instance genericNumber :: Generic (Prim.Number)

    instance genericString :: Generic (Prim.String)

    instance showTm :: Show (Tm)

    instance showTy :: Show (Ty)


### Values

    cast :: forall b. forall a. (Generic (a),Generic (b)) => a -> Maybe b

    elementProxy :: forall a. Proxy [a] -> Proxy a

    everything :: forall r. forall a. (Generic (a)) => (r -> r -> r) -> GenericQ r -> a -> r

    everythingImpl :: forall r. forall a. (r -> r -> r) -> GenericQ r -> Tm -> r

    everywhere :: forall a. (Generic (a)) => GenericT -> a -> a

    everywhereImpl :: GenericT -> Tm -> Tm

    gmapT :: forall a. (Generic (a)) => GenericT -> a -> a

    gmapTImpl :: GenericT -> Tm -> Tm

    gshow :: forall a. (Generic (a)) => a -> Prim.String

    gsize :: forall a. (Generic (a)) => a -> Prim.Number

    mkQ :: forall r. forall a. (Generic (a)) => r -> (a -> r) -> GenericQ r

    mkT :: forall a. (Generic (a)) => (a -> a) -> GenericT

    runGenericQ :: forall r. GenericQ r -> Tm -> r

    runGenericT :: GenericT -> Tm -> Tm

    sizeOf :: Tm -> Prim.Number




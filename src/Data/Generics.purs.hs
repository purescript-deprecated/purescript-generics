module Data.Generics where

import Prelude
import Data.Array
import Data.Either
import Data.Maybe
import Data.Tuple
import Data.Foldable
import Data.Traversable

data Ty
  = TyNum
  | TyStr
  | TyBool
  | TyArr Ty
  | TyObj [{ key :: String, value :: Ty }]
  | TyCon { tyCon :: String, args :: [Ty] }

instance showTy :: Show Ty where
  show TyNum = "Number"
  show TyStr = "String"
  show TyBool = "Boolean"
  show (TyArr el) = "[" ++ show el ++ "]"
  show (TyObj fs) = "{ " ++ joinWith (map (\f -> f.key ++ " :: " ++ show f.value) fs) ", " ++ " }"
  show (TyCon c) = joinWith (c.tyCon : map (\ty -> "(" ++ show ty ++ ")") c.args) " "
  
instance eqTy :: Eq Ty where
  (==) TyNum       TyNum       = true
  (==) TyStr       TyStr       = true
  (==) TyBool      TyBool      = true
  (==) (TyObj es1) (TyObj es2) = all id (zipWith eqObjEntry es1 es2)
  (==) (TyCon { tyCon = ty1, args = as1 }) 
       (TyCon { tyCon = ty2, args = as2 }) = ty1 == ty2 && as1 == as2
  (==) _ _ = false
  (/=) x y = not (x == y)

data Tm 
  = TmNum Number
  | TmStr String
  | TmBool Boolean
  | TmArr [Tm]
  | TmObj [{ key :: String, value :: Tm }]
  | TmCon { con :: String, values :: [Tm] }

instance showTm :: Show Tm where
  show (TmNum n) = show n
  show (TmStr s) = show s
  show (TmBool b) = show b
  show (TmArr arr) = "[" ++ joinWith (map show arr) ", " ++ "]"
  show (TmObj fs) = "{ " ++ joinWith (map (\f -> f.key ++ ": " ++ show f.value) fs) ", " ++ " }"
  show (TmCon c) = joinWith (c.con : map (\tm -> "(" ++ show tm ++ ")") c.values) " "
  
instance eqTm :: Eq Tm where
  (==) (TmNum n1)   (TmNum n2)   = n1 == n2
  (==) (TmStr s1)   (TmStr s2)   = s1 == s2
  (==) (TmBool b1)  (TmBool b2)  = b1 == b2
  (==) (TmArr arr1) (TmArr arr2) = arr1 == arr2
  (==) (TmObj es1)  (TmObj es2)  = all id (zipWith eqObjEntry es1 es2)
  (==) (TmCon { con = t1, values = vs1 }) 
       (TmCon { con = t2, values = vs2 }) = t1 == t2 && vs1 == vs2
  (==) _ _ = false
  (/=) x y = not (x == y)
  
eqObjEntry :: forall k v. (Eq k, Eq v) => { key :: k, value :: v } -> { key :: k, value :: v } -> Boolean
eqObjEntry { key = k1, value = v1 }
           { key = k2, value = v2 } = k1 == k2 && v1 == v2
  
data Proxy a = Proxy

class Generic a where
  typeOf :: Proxy a -> Ty
  term :: a -> Tm
  unTerm :: Tm -> Maybe a
  
instance genericNumber :: Generic Number where
  typeOf _ = TyNum
  term = TmNum
  unTerm (TmNum n) = Just n
  unTerm _ = Nothing
  
instance genericString :: Generic String where
  typeOf _ = TyStr
  term = TmStr
  unTerm (TmStr s) = Just s
  unTerm _ = Nothing
  
instance genericBoolean :: Generic Boolean where
  typeOf _ = TyBool
  term = TmBool
  unTerm (TmBool b) = Just b
  unTerm _ = Nothing
 
elementProxy :: forall a. Proxy [a] -> Proxy a
elementProxy _ = Proxy
 
instance genericArray :: (Generic a) => Generic [a] where
  typeOf p = TyArr (typeOf (elementProxy p))
  term arr = TmArr $ map term arr
  unTerm (TmArr arr) = traverse unTerm arr
  unTerm _ = Nothing
  
fstProxy :: forall a b. Proxy (Tuple a b) -> Proxy a
fstProxy _ = Proxy

sndProxy :: forall a b. Proxy (Tuple a b) -> Proxy b
sndProxy _ = Proxy
  
instance genericTuple :: (Generic a, Generic b) => Generic (Tuple a b) where
  typeOf p = TyCon { tyCon: "Data.Tuple.Tuple", args: [typeOf $ fstProxy p, typeOf $ sndProxy p] }
  term (Tuple x y) = TmCon { con: "Data.Tuple.Tuple", values: [term x, term y] }
  unTerm (TmCon { con = "Data.Tuple.Tuple", values = [x, y] }) = Tuple <$> unTerm x <*> unTerm y
  unTerm _ = Nothing
  
maybeProxy :: forall a. Proxy (Maybe a) -> Proxy a
maybeProxy _ = Proxy
  
instance genericMaybe :: (Generic a) => Generic (Maybe a) where
  typeOf p = TyCon { tyCon: "Data.Maybe.Maybe", args: [typeOf $ maybeProxy p] }
  term (Just x) = TmCon { con: "Data.Maybe.Just", values: [term x] }
  term Nothing = TmCon { con: "Data.Maybe.Nothing", values: [] }
  unTerm (TmCon { con = "Data.Maybe.Just", values = [x] }) = Just <$> unTerm x
  unTerm (TmCon { con = "Data.Maybe.Nothing" }) = Just Nothing
  unTerm _ = Nothing
  
leftProxy :: forall a b. Proxy (Either a b) -> Proxy a
leftProxy _ = Proxy

rightProxy :: forall a b. Proxy (Either a b) -> Proxy b
rightProxy _ = Proxy
  
instance genericEither :: (Generic a, Generic b) => Generic (Either a b) where
  typeOf p = TyCon { tyCon: "Data.Either.Either", args: [typeOf $ leftProxy p, typeOf $ rightProxy p] }
  term (Left l) = TmCon { con: "Data.Either.Left", values: [term l] }
  term (Right r) = TmCon { con: "Data.Either.Right", values: [term r] }
  unTerm (TmCon { con = "Data.Either.Left", values = [l] }) = Left <$> unTerm l
  unTerm (TmCon { con = "Data.Either.Right", values = [r] }) = Right <$> unTerm r
  unTerm _ = Nothing

-- |
-- Generic size
--

sizeOf :: Tm -> Number
sizeOf (TmArr arr) = foldl (+) 0 (map sizeOf arr)
sizeOf (TmObj obj) = foldl (+) 0 (map (\p -> sizeOf p.value) obj)
sizeOf (TmCon con) = foldl (+) 0 (map sizeOf con.values)
sizeOf _ = 1

gsize :: forall a. (Generic a) => a -> Number
gsize a = sizeOf (term a)

-- |
-- Generic Show
--

gshow :: forall a. (Generic a) => a -> String
gshow a = show (term a)

-- |
-- Generic equality
--

geq :: forall a. (Generic a) => a -> a -> Boolean
geq a b = (term a) == (term b)

-- |
-- Generic cast
--

cast :: forall a b. (Generic a, Generic b) => a -> Maybe b
cast a = unTerm (term a)

-- |
-- Generic transformations
--

data GenericT = GenericT (Tm -> Tm)

runGenericT :: GenericT -> Tm -> Tm
runGenericT (GenericT f) tm = f tm

mkT :: forall a. (Generic a) => (a -> a) -> GenericT
mkT f = GenericT $ \t -> fromMaybe t $ do
  a <- unTerm t
  return $ term (f a)

gmapTImpl :: GenericT -> Tm -> Tm
gmapTImpl f (TmArr arr) = TmArr $ map (runGenericT f) arr
gmapTImpl f (TmObj fs) = TmObj $ map (\p -> { key: p.key, value: runGenericT f (p.value) }) fs
gmapTImpl f (TmCon c) = TmCon { con: c.con, values: map (runGenericT f) c.values }
gmapTImpl _ other = other

gmapT :: forall a. (Generic a) => GenericT -> a -> a
gmapT f a = case unTerm (gmapTImpl f (term a)) of
  Just a -> a

everywhereImpl :: GenericT -> Tm -> Tm
everywhereImpl f (TmArr arr) = runGenericT f $ TmArr $ map (everywhereImpl f) arr
everywhereImpl f (TmObj fs) = runGenericT f $ TmObj $ map (\p -> { key: p.key, value: everywhereImpl f p.value }) fs
everywhereImpl f (TmCon c) = runGenericT f $ TmCon { con: c.con, values: map (everywhereImpl f) c.values }
everywhereImpl f other = runGenericT f other

everywhere :: forall a. (Generic a) => GenericT -> a -> a
everywhere f a = case unTerm (everywhereImpl f (term a)) of
  Just a -> a

-- |
-- Generic queries
--

data GenericQ r = GenericQ (Tm -> r)

runGenericQ :: forall r. GenericQ r -> Tm -> r
runGenericQ (GenericQ f) tm = f tm

mkQ :: forall a r. (Generic a) => r -> (a -> r) -> GenericQ r
mkQ r f = GenericQ $ \t -> fromMaybe r $ do
  a <- unTerm t
  return $ f a

everythingImpl :: forall a r. (r -> r -> r) -> GenericQ r -> Tm -> r
everythingImpl (+) f t@(TmArr arr) = foldl (+) (runGenericQ f t) (map (everythingImpl (+) f) arr)
everythingImpl (+) f t@(TmObj fs) = foldl (+) (runGenericQ f t) (map (\p -> everythingImpl (+) f p.value) fs)
everythingImpl (+) f t@(TmCon c) = foldl (+) (runGenericQ f t) (map (everythingImpl (+) f) c.values)
everythingImpl _ f other = runGenericQ f other

everything :: forall a r. (Generic a) => (r -> r -> r) -> GenericQ r -> a -> r
everything (+) f a = everythingImpl (+) f (term a)


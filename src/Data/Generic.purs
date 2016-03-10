module Data.Generic
  ( Generic
  , toSpine
  , toSignature
  , fromSpine
  , GenericSpine(..)
  , GenericSignature(..)
  , showSignature
  , DataConstructor()
  , showDataConstructor
  , isValidSpine
  , gShow
  , gEq
  , gCompare
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Traversable (traverse)
import Data.Foldable (all, and, find, fold, intercalate)
import Data.Array (null, length, sortBy, zipWith)
import Data.String (joinWith)
import Type.Proxy (Proxy(..))

-- | A GenericSpine is a universal representation of an arbitrary data structure (that does not contain function arrows).
data GenericSpine = SProd String (Array (Unit -> GenericSpine))
                  | SRecord (Array { recLabel :: String, recValue :: Unit -> GenericSpine })
                  | SNumber Number
                  | SBoolean Boolean
                  | SInt Int
                  | SString String
                  | SChar Char
                  | SArray (Array (Unit -> GenericSpine))

-- | Identifies a data constructor.
type DataConstructor =
  { sigConstructor :: String
  , sigValues :: Array (Unit -> GenericSignature)
  }

-- | A GenericSignature is a universal representation of the structure of an arbitrary data structure (that does not contain function arrows).
data GenericSignature = SigProd String (Array DataConstructor)
                      | SigRecord (Array { recLabel :: String, recValue :: Unit -> GenericSignature })
                      | SigNumber
                      | SigBoolean
                      | SigInt
                      | SigString
                      | SigChar
                      | SigArray (Unit -> GenericSignature)

showSignature :: GenericSignature -> String
showSignature sig =
  fold $ case sig of
    SigProd tyName ctors ->
      ["SigProd ", show tyName, " ", showArray showDataConstructor ctors]
    SigRecord labels ->
      ["SigRecord ", showArray showLabel labels]
    SigNumber ->
      ["SigNumber"]
    SigBoolean ->
      ["SigBoolean"]
    SigInt ->
      ["SigInt"]
    SigString ->
      ["SigString"]
    SigChar ->
      ["SigChar"]
    SigArray sig' ->
      ["SigArray ", paren (force sig')]

  where
  paren s
    | needsParen s = "(" <> showSignature s <> ")"
    | otherwise = showSignature s

  needsParen s = case s of
    SigProd _ _ -> true
    SigRecord _ -> true
    SigNumber   -> false
    SigBoolean  -> false
    SigInt      -> false
    SigString   -> false
    SigChar     -> false
    SigArray _  -> true

instance showGenericSignature :: Show GenericSignature where
  show = showSignature

-- We use this instead of the default Show Array instance to avoid escaping
-- strings twice.
showArray :: forall a. (a -> String) -> Array a -> String
showArray f xs = "[ " <> intercalate ", " (map f xs) <> " ]"

showLabel ::
  { recLabel :: String, recValue :: Unit -> GenericSignature } -> String
showLabel l =
  "{ recLabel: " <> show l.recLabel <>
  ", recValue: " <> showSignature (force l.recValue) <>
  " }"

showDataConstructor :: DataConstructor -> String
showDataConstructor dc =
  "{ sigConstructor: " <> show dc.sigConstructor <>
  ", sigValues: " <> showArray (showSignature <<< force) dc.sigValues <>
  "}"

force :: forall a. (Unit -> a) -> a
force = ($ unit)

anyProxy :: forall a. Proxy a
anyProxy = Proxy

-- | The Generic typeclass provides methods for sending data to/from spine representations, as well as querying about the signatures of spine representations.
-- | For standard data structures, you can simply write `derive instance genericFoo :: Generic Foo` in the module they are declared, and the instance methods will be filled in for you.
class Generic a where
    toSpine :: a -> GenericSpine
    toSignature :: Proxy a -> GenericSignature
    fromSpine :: GenericSpine -> Maybe a

-- | Checks that the spine follows the structure defined by the signature
isValidSpine :: GenericSignature -> GenericSpine -> Boolean
isValidSpine SigBoolean (SBoolean _) = true
isValidSpine SigNumber (SNumber _) = true
isValidSpine SigInt (SInt _) = true
isValidSpine SigString (SString _) = true
isValidSpine SigChar (SChar _) = true
isValidSpine (SigArray sig) (SArray spines) = all (isValidSpine (sig unit) <<< (unit #)) spines
isValidSpine (SigProd _ alts) (SProd tag values) =
  case find ((tag ==) <<< _.sigConstructor) alts of
    Nothing -> false
    Just { sigValues: sigValues } ->
      and $ zipWith (\sig spine -> isValidSpine (sig unit) (spine unit)) sigValues values
isValidSpine (SigRecord fieldSigs) (SRecord fieldVals) =
  and $ zipWith (\sig val -> isValidSpine (sig.recValue unit) (val.recValue unit))
                (sortBy (\a b -> compare a.recLabel b.recLabel) fieldSigs)
                (sortBy (\a b -> compare a.recLabel b.recLabel) fieldVals)
isValidSpine _ _ = false

instance genericNumber :: Generic Number where
    toSpine x = SNumber x
    toSignature _ = SigNumber
    fromSpine (SNumber n) = Just n
    fromSpine _ = Nothing

instance genericInt :: Generic Int where
    toSpine x = SInt x
    toSignature _ = SigInt
    fromSpine (SInt n) = Just n
    fromSpine _ = Nothing

instance genericString :: Generic String where
    toSpine x = SString x
    toSignature _ = SigString
    fromSpine (SString s) = Just s
    fromSpine _ = Nothing

instance genericChar :: Generic Char where
    toSpine x = SChar x
    toSignature _ = SigChar
    fromSpine (SChar s) = Just s
    fromSpine _ = Nothing

instance genericBool :: Generic Boolean where
    toSpine b = SBoolean b
    toSignature _ = SigBoolean
    fromSpine (SBoolean b)  = Just b
    fromSpine _ = Nothing

instance genericArray :: (Generic a) => Generic (Array a) where
    toSpine xs = SArray ((\x y -> toSpine x) <$> xs)
    toSignature x = SigArray (\unit -> toSignature (lowerProxy x))
        where lowerProxy :: Proxy (Array a) -> Proxy a
              lowerProxy Proxy = (anyProxy :: Proxy a)

    fromSpine (SArray x) = traverse (fromSpine <<< ($ unit)) x
    fromSpine _ = Nothing

instance genericTuple :: (Generic a, Generic b) => Generic (Tuple a b) where
      toSpine (Tuple x y) = SProd "Data.Tuple.Tuple" [\u -> toSpine x, \u -> toSpine y]

      toSignature x = SigProd "Data.Tuple.Tuple"
                              [ { sigConstructor: "Data.Tuple.Tuple"
                                , sigValues: [\u -> toSignature (fstProxy x), \u -> toSignature (sndProxy x)]
                                }
                              ]
                    where fstProxy :: Proxy (Tuple a b) -> Proxy a
                          fstProxy Proxy = (anyProxy :: Proxy a)
                          sndProxy :: Proxy (Tuple a b) -> Proxy b
                          sndProxy Proxy = (anyProxy :: Proxy b)

      fromSpine (SProd "Data.Tuple.Tuple" [x,y]) = Tuple <$> fromSpine (x unit) <*> fromSpine (y unit)
      fromSpine _ = Nothing

instance genericMaybe :: (Generic a) => Generic (Maybe a) where
      toSpine (Just x) = SProd "Data.Maybe.Just" [\u -> toSpine x]
      toSpine Nothing = SProd "Data.Maybe.Nothing" []
      toSignature x = SigProd "Data.Maybe.Maybe"
                              [ { sigConstructor: "Data.Maybe.Just"
                                , sigValues: [\u -> toSignature (mbProxy x)]
                                }
                              , { sigConstructor: "Data.Maybe.Nothing"
                                , sigValues: []
                                }
                              ]
          where mbProxy :: Proxy (Maybe a) -> Proxy a
                mbProxy Proxy = (anyProxy :: Proxy a)
      fromSpine (SProd "Data.Maybe.Just" [x]) = Just <$> fromSpine (x unit)
      fromSpine (SProd "Data.Maybe.Nothing" []) = return Nothing
      fromSpine _ = Nothing

instance genericEither :: (Generic a, Generic b) => Generic (Either a b) where
    toSpine (Left x) = SProd "Data.Either.Left" [\u -> toSpine x]
    toSpine (Right x) = SProd "Data.Either.Right" [\u -> toSpine x]
    toSignature x = SigProd "Data.Either.Either"
                            [ { sigConstructor: "Data.Either.Left"
                              , sigValues: [\u -> toSignature (lproxy x)]
                              }
                            , { sigConstructor: "Data.Either.Right"
                              , sigValues: [\u -> toSignature (rproxy x)]
                              }
                            ]
          where lproxy :: Proxy (Either a b) -> Proxy a
                lproxy Proxy = (anyProxy :: Proxy a)
                rproxy :: Proxy (Either a b) -> Proxy b
                rproxy Proxy = (anyProxy :: Proxy b)
    fromSpine (SProd "Data.Either.Left"  [x]) = Left  <$> fromSpine (x unit)
    fromSpine (SProd "Data.Either.Right" [x]) = Right <$> fromSpine (x unit)
    fromSpine _ = Nothing


--- Generic Functions
genericShowPrec :: Int -> GenericSpine -> String
genericShowPrec d (SProd s arr) =
    if null arr
    then s
    else showParen (d > 10) $ s <> " " <> joinWith " " (map (\x -> genericShowPrec 11 (x unit)) arr)
  where showParen false x = x
        showParen true  x = "(" <> x <> ")"

genericShowPrec d (SRecord xs) = "{" <> joinWith ", " (map (\x -> x.recLabel <> ": " <> genericShowPrec 0 (x.recValue unit)) xs) <> "}"
genericShowPrec d (SBoolean x) = show x
genericShowPrec d (SInt x)     = show x
genericShowPrec d (SNumber x)  = show x
genericShowPrec d (SString x)  = show x
genericShowPrec d (SChar x)    = show x
genericShowPrec d (SArray xs)  = "[" <> joinWith ", "  (map (\x -> genericShowPrec 0 (x unit)) xs) <> "]"

-- | This function can be used as the default instance for Show for any instance of Generic
gShow :: forall a. (Generic a) => a -> String
gShow = genericShowPrec 0 <<< toSpine

foreign import zipAll :: forall a b. (a -> b -> Boolean) -> Array a -> Array b -> Boolean

instance eqGeneric :: Eq GenericSpine where
    eq (SProd s1 arr1) (SProd s2 arr2) = s1 == s2
                                      && length arr1 == length arr2
                                      && zipAll (\x y -> x unit == y unit) arr1 arr2
    eq (SRecord xs) (SRecord ys) = length xs == length ys && zipAll go xs ys
             where go x y = x.recLabel == y.recLabel && x.recValue unit == y.recValue unit
    eq (SNumber x)  (SNumber y)  = x == y
    eq (SBoolean x) (SBoolean y) = x == y
    eq (SInt x)     (SInt y)     = x == y
    eq (SString x)  (SString y)  = x == y
    eq (SChar x)    (SChar y)    = x == y
    eq (SArray xs)  (SArray ys)  = length xs == length ys && zipAll (\x y -> x unit == y unit) xs ys
    eq _ _ = false

eqDataConstructor :: DataConstructor -> DataConstructor -> Boolean
eqDataConstructor p1 p2 = p1.sigConstructor == p2.sigConstructor
                       && zipAll (\x y -> x unit == y unit) p1.sigValues p2.sigValues

instance eqGenericSignature :: Eq GenericSignature where
  eq (SigProd s1 arr1) (SigProd s2 arr2) = s1 == s2
                                        && length arr1 == length arr2
                                        && zipAll eqDataConstructor arr1 arr2
  eq (SigRecord arr1) (SigRecord arr2)   = length arr1 == length arr2
                                        && zipAll (\x y -> x.recLabel == y.recLabel) arr1 arr2
                                        && zipAll (\x y -> x.recValue unit == y.recValue unit) arr1 arr2
  eq SigNumber   SigNumber               = true
  eq SigBoolean  SigBoolean              = true
  eq SigInt      SigInt                  = true
  eq SigString   SigString               = true
  eq SigChar     SigChar                 = true
  eq (SigArray t1) (SigArray t2)         = t1 unit == t2 unit
  eq _ _                                 = false

-- | This function can be used as the default instance for Eq for any instance of Generic
gEq :: forall a. (Generic a) => a -> a -> Boolean
gEq x y = toSpine x == toSpine y

foreign import zipCompare :: forall a b. (a -> b -> Int) -> Array a -> Array b -> Int

instance ordGeneric :: Ord GenericSpine where
    compare (SProd s1 arr1) (SProd s2 arr2) =
        case compare s1 s2 of
          EQ -> compare 0 $ zipCompare (\x y -> case compare (x unit) (y unit) of
                                                  EQ -> 0
                                                  LT -> 1
                                                  GT -> -1) arr1 arr2
          c1 -> c1
    compare (SProd _ _) _ = LT
    compare _ (SProd _ _) = GT
    compare (SRecord xs) (SRecord ys) = compare 0 $ zipCompare go xs ys
        where go x y = case compare x.recLabel y.recLabel of
                         EQ -> case compare (x.recValue unit) (y.recValue unit) of
                                 EQ -> 0
                                 LT -> 1
                                 GT -> -1
                         LT -> 1
                         GT -> -1
    compare (SRecord _) _ = LT
    compare _ (SRecord _) = GT
    compare (SInt x) (SInt y) = compare x y
    compare (SInt _) _ = LT
    compare _ (SInt _) = GT
    compare (SBoolean x) (SBoolean y) = compare x y
    compare (SBoolean _) _ = LT
    compare _ (SBoolean _) = GT
    compare (SNumber x) (SNumber y) = compare x y
    compare (SNumber _) _ = LT
    compare _ (SNumber _) = GT
    compare (SString x) (SString y) = compare x y
    compare (SString _) _ = LT
    compare _ (SString _) = GT
    compare (SChar x) (SChar y) = compare x y
    compare (SChar _) _ = LT
    compare _ (SChar _) = GT
    compare (SArray xs) (SArray ys) = compare 0 $ zipCompare (\x y -> case compare (x unit) (y unit) of
                                                                        EQ -> 0
                                                                        LT -> 1
                                                                        GT -> -1) xs ys

-- | This function can be used as the default instance for the compare method of Ord for any instance of Generic
gCompare :: forall a. (Generic a) => a -> a -> Ordering
gCompare x y = compare (toSpine x) (toSpine y)

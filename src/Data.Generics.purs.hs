module Data.Generics where

import Prelude
import Data.Maybe
import Data.Array
import Control.Monad

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
  unTerm (TmArr arr) = mapM unTerm arr
  unTerm _ = Nothing


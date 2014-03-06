module Main where

import Prelude
import Data.Generics
import Data.Maybe
import Data.Array
import Data.String
import Control.Monad.Eff

data Expr 
  = Var String
  | Lam String Expr
  | App Expr Expr

instance genericExpr :: Generic Expr where
  typeOf _ = TyCon { tyCon: "Main.Expr", args: [] }
  term (Var s) = TmCon { con: "Main.Var", values: [term s] }
  term (Lam s e) = TmCon { con: "Main.Lam", values: [term s, term e] }
  term (App e1 e2) = TmCon { con: "Main.App", values: [term e1, term e2] }
  unTerm (TmCon { con = "Main.Var", values = [s] }) = Var <$> unTerm s
  unTerm (TmCon { con = "Main.Lam", values = [s, e] }) = Lam <$> unTerm s <*> unTerm e
  unTerm (TmCon { con = "Main.App", values = [e1, e2] }) = App <$> unTerm e1 <*> unTerm e2
  unTerm _ = Nothing

s :: Expr
s = Lam "x" $ Lam "y" $ Lam "z" $ App (App (Var "x") (Var "z")) (App (Var "y") (Var "z"))

main = do
  Debug.Trace.trace $ show $ typeOf (Proxy :: Proxy Expr)
  -- Generic show
  Debug.Trace.trace $ gshow s
  -- Generic size
  Debug.Trace.trace $ show $ gsize s
  -- Make all Vars upper case
  Debug.Trace.trace $ gshow $ everywhere (mkT toUpper) s
  -- Count the occurrences of Var
  Debug.Trace.trace $ show $ everything concat (mkQ [] \e -> case e of 
    Var v -> [v]
    _ -> []) s

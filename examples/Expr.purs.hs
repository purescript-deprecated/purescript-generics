module Main where

import Prelude
import Data.Generics
import Data.Maybe
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

main = do
  Debug.Trace.trace $ show $ typeOf (Proxy :: Proxy Expr)
  Debug.Trace.trace $ show $ term (Lam "x" $ Var "x")

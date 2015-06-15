module Andromeda.Lambda.Core where

-- TODO: Elab to core expression, then to String
--       instead of directly from 'Expr' to String

data CExpr =
    Assign String CExpr
  | Call String
  | DeclLocal String CExpr

data CType = Scalar

data CStmt = Assign

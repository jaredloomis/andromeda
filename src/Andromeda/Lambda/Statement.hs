{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Andromeda.Lambda.Statement where

import GHC.Stack (errorWithStackTrace)

import Andromeda.Lambda.Expr
import Andromeda.Lambda.Type
import Andromeda.Lambda.Glom

{-
data Statement (i :: [*]) a where
    Assign :: Pat b -> Expr b -> Statement '[b] ()
    Then   :: Statement ia a -> Statement ib b -> Statement (ia '++ ib) b

instance HasGLSL (Statement i a) where
    toGLSL (Assign  UnitG _)         = ""
    toGLSL (Assign (BaseG (V n _)) val) =
        n ++ " = " ++ toGLSL val ++ ";\n"
    toGLSL (Assign (a `PairG` b) val) =
        let (l, r) = unPair val
        in toGLSL (Assign a l) ++
           toGLSL (Assign b r)

unPair :: forall a b. (HasType a, HasType b) =>
    Expr (a, b) -> (Expr a, Expr b)
unPair = unPair' . betaReduce

unPair' :: forall a b. (HasType a, HasType b) =>
    Expr (a, b) -> (Expr a, Expr b)
unPair' (Lit Pair :$ a :$ b) = (a, b)
unPair' (Var (V name _)) =
    case pat name :: Pat (a, b) of
        PairG (BaseG varA) (BaseG varB) ->
            (Var varA, Var varB)
        _ -> errorWithStackTrace
            "unPair': 'pat' returned an unexpected format."
unPair' expr = errorWithStackTrace $
    "unPair: Pair was made through some " ++
    "unknown operation:\n" ++ show expr

--compileStmt :: Statment a -> 

-}

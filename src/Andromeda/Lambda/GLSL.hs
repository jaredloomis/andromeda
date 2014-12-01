{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
module Andromeda.Lambda.GLSL where

import GHC.Stack (errorWithStackTrace)

import Control.Monad.Free

import Andromeda.Lambda.Glom
import Andromeda.Lambda.Expr
import Andromeda.Lambda.Type

data Qualifier = In | Out | Uniform

instance HasGLSL Qualifier where
    toGLSL In = "in"
    toGLSL Out = "out"
    toGLSL Uniform = "uniform"

data Statement next =
    Assign Bind next
  deriving (Functor)

type Stmt a = Free Statement a

instance HasGLSL next => HasGLSL (Statement next) where
    toGLSL (Assign b next) = toGLSL b ++ "\n" ++ toGLSL next
--    toGLSL Done = ""
--    toGLSL (Then a b) = toGLSL a ++ "\n" ++ toGLSL b

instance HasGLSL (Stmt ()) where
    toGLSL (Free x) = toGLSL x
    toGLSL (Pure ()) = ""

data Bind = forall a. HasGLSL a => Bind (Pat a) (Expr a)

instance HasGLSL Bind where
    toGLSL (Bind UnitG _) = ""
    toGLSL (Bind (l `PairG` r) e) =
        let (a,b) = unPair e
        in toGLSL (Bind l a) ++ "\n" ++ toGLSL (Bind r b)
    toGLSL (Bind (BaseG (V name _)) e) =
--        toGLSL ty ++ " " ++
        name ++ " = " ++ toGLSL e ++ ";\n"

data Declaration = forall a. Declaration Qualifier (Pat a)

instance HasGLSL Declaration where
    toGLSL (Declaration q patt) = showPat patt
      where
        showPat :: Pat t -> String
        showPat UnitG = ""
        showPat (l `PairG` r) = showPat l ++ "\n" ++ showPat r
        showPat (BaseG (V name ty)) =
            toGLSL q ++ " " ++ toGLSL ty ++ " " ++ name ++ ";\n"

data Param = forall n a. Param (Vect n a) String

data Definition =
    forall n a. Definition
        (Maybe (Vect n a))  -- Return type ('Nothing' is void)
        String              -- Name
        --[Param]
        (Free Statement ())

instance HasGLSL Definition where
    toGLSL (Definition mty name body) =
        maybe "void" toGLSL mty ++ " " ++ name ++
        "() {\n" ++ toGLSL body ++ "}\n"

infix 1 =:
(=:) :: (HasType a, HasGLSL a) => Pat a -> Expr a -> Stmt ()
(=:) p e = Free $ Assign (Bind p e) (Pure ())

------------
-- Unpair --
------------

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

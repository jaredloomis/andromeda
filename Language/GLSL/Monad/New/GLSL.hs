{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module GLSL where

import GHC.Stack (errorWithStackTrace)
import Unsafe.Coerce (unsafeCoerce)

import HasGLSL
import Glom
import Expr
import Type

data Qualifier = In | Out | Uniform

instance HasGLSL Qualifier where
    toGLSL In = "in"
    toGLSL Out = "out"
    toGLSL Uniform = "uniform"

data Statement =
    Assign Bind

instance HasGLSL Statement where
    toGLSL (Assign b) = toGLSL b

data Bind = forall a. HasGLSL a => Bind (Pat a) (Expr a)

instance HasGLSL Bind where
    toGLSL (Bind UnitG _) = ""
    toGLSL (Bind (l :* r) e) =
        let (a,b) = unPair e
        in toGLSL (Bind l a) ++ "\n" ++ toGLSL (Bind r b)
    toGLSL (Bind (BaseG (V name ty)) e) =
        toGLSL ty ++ " " ++ -- XXX: ????
        name ++ " = " ++ toGLSL e ++ ";\n"

data Declaration = forall a. Declaration Qualifier (Pat a)

instance HasGLSL Declaration where
    toGLSL (Declaration q patt) = showPat patt
      where
        showPat :: Pat t -> String
        showPat UnitG = ""
        showPat (l :* r) = showPat l ++ "\n" ++ showPat r
        showPat (BaseG (V name ty)) =
            toGLSL q ++ " " ++ toGLSL ty ++ " " ++ name ++ ";\n"

data Param = forall n a. Param (Vect n a) String

data Definition =
    forall n a. Definition
        (Maybe (Vect n a))  -- Return type ('Nothing' is void)
        String              -- Name
        --[Param]
        Statement

instance HasGLSL Definition where
    toGLSL (Definition mty name body) =
        maybe "void" toGLSL mty ++ " " ++ name ++
        "() {\n" ++ toGLSL body ++ "}\n"

infix 1 =:
(=:) :: (HasType a, HasGLSL a) => Pat a -> Expr a -> Statement
(=:) p e = Assign (Bind p e)

------------
-- Unpair --
------------

pattern PairFun t1 t2 tf ts a b = Lit (PrimOp (
        t1 :->:
        t2 :->:
        (tf :*: ts)
        )) :$ a :$ b

unPair :: forall a b. (HasGLSL a, HasGLSL b) =>
    Expr (a, b) -> (Expr a, Expr b)
unPair = unPair' . betaReduce

-- TODO: Completely rewrite Pair system, it's awkward.
unPair' :: forall a b. (HasGLSL a, HasGLSL b) =>
    Expr (a, b) -> (Expr a, Expr b)
unPair' (Lit Pair :$ a :$ b) = (a, b)
unPair' (PairFun (_ :: Type aat) (_ :: Type abt)
                (_ :: Type rat) (_ :: Type rbt)
                (apa  :: Expr apt) (apb  :: Expr bpt)) =
    let aatTy = typeOf (undefined :: aat)
        abtTy = typeOf (undefined :: abt)
        raTy = typeOf (undefined :: rat)
        rbTy = typeOf (undefined :: rbt)
        aTy = typeOf (undefined :: a)
        bTy = typeOf (undefined :: b)
        apaTy = typeOf (undefined :: apt)
        apbTy = typeOf (undefined :: bpt)
    in if aatTy == apaTy && abtTy == apbTy &&
          raTy == unsafeCoerce apaTy && rbTy == unsafeCoerce apbTy &&
          aTy  == unsafeCoerce apaTy && bTy  == unsafeCoerce apbTy
            then (unsafeCoerce apa, unsafeCoerce apb)
        else errorWithStackTrace "unPair: Types are not all equal."
unPair' (Lam f :$ x) = unPair' $ f x
unPair' _ = errorWithStackTrace $ "unPair: Pair was made through some " ++
                                 "unknown operation."

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
module Expr where

import GHC.Stack (errorWithStackTrace)
import Data.Ratio (numerator, denominator)
import Data.String (IsString(..))
import Data.List (intercalate)
import Control.Arrow (second)

import HasGLSL
import Type
import Glom

-- | Lambda calculus with HOAS.
data Expr a where
    Var :: V a -> Expr a
    Lit :: Lit a -> Expr a
    Vam :: V a -> Expr b -> Expr (a -> b)
    Lam :: (Expr a -> Expr b) -> Expr (a -> b)
    (:$) :: Expr (a -> b) -> Expr a -> Expr b
infixl 9 :$

instance Show (Expr a) where
    show (Var v) = "Var (" ++ show v ++ ")"
    show (Lit l) = "Lit (" ++ show l ++ ")"
    show (Lam _) = "Lam (....)"
    show (a:$ b) = "(" ++ show a ++ ") :$ (" ++ show b ++ ")"
    show (Vam v e) = "Vam (" ++ show v ++ ") (" ++ show e ++ ")"

instance (Num a, HasGLSL a, HasType a) => Num (Expr a) where
    fromInteger int =
        Lit . Literal $ (fromInteger int :: a)
    (+) a b = Lit (BinOp "+") :$ a :$ b
    (-) a b = Lit (BinOp "-") :$ a :$ b
    (*) a b = Lit (BinOp "*") :$ a :$ b
    abs x = Lit (Native "abs") :$ x
    signum = error "Called signum on 'Expr a'."

instance (Fractional a, HasGLSL a, HasType a) => Fractional (Expr a) where
    fromRational rat = Lit (BinOp "/") :$
                       (fromInteger (numerator rat) :: Expr Int) :$
                       (fromInteger (denominator rat) :: Expr Int)
    n / d = Lit (BinOp "/") :$ n :$ d

instance HasGLSL (Expr a) where
    toGLSL (Var (V n _)) = n
    toGLSL (Lit lit) = toGLSL lit
    toGLSL (Vam v e) = toGLSL v ++ " = " ++ toGLSL e
    toGLSL (Lam f :$ x) = toGLSL $ f x
    toGLSL app@(_ :$ _) =
        let (func, args) = collectArgs app
            argsStr = argsToStr args
        in case func of
            ExprN (Lit lit) ->
                case lit of
                    UnOp f  -> f ++ assertArgLen args 1 (paren argsStr)
                    BinOp f -> assertArgLen args 2 $
                        paren (toGLSL (head args))
                        ++ " " ++ f ++ " " ++
                        paren (toGLSL (last args))
                    FieldAccess f -> assertArgLen args 1 $
                        paren (toGLSL (head args)) ++ "." ++ f
                    f       -> toGLSL (Lit f) ++ paren argsStr
            ExprN (Var (V n t)) -> toGLSL (Var (V n t))
            ExprN (Lam _) -> errorWithStackTrace $
                "toGLSL Expr: recieved Lam. Expr must be" ++
                " beta reduced. Use 'betaReduce'."
            ExprN _ -> errorWithStackTrace $
                "Error in toGLSL Expr: case didn't match on " ++
                "Lit, Var or Lam, and the other cases are impossible!"
      where
        assertArgLen args len x
            | length args /= len =
                errorWithStackTrace $
                    "Error in 'assertArgLen': wanted: " ++ show len ++
                    ", recieved: " ++ show (length args)
            | otherwise = x
    toGLSL (Lam _) =
        errorWithStackTrace "Error in toGLSL Expr: recieved unapplied Lam."

-- Helpers for HasGLSL Expr instance --

-- | Existential Expr.
data ExprN = forall a. ExprN (Expr a)
instance HasGLSL ExprN where
    toGLSL (ExprN e) = toGLSL e

collectArgsR :: Expr a -> (ExprN, [ExprN])
collectArgsR (xs :$ x) =
    let (f,as) = collectArgsR xs
    in (f, ExprN x : as)
collectArgsR x = (ExprN x,[])

argList :: Expr a -> String
argList = argsToStr . snd . collectArgs

collectArgs :: Expr a -> (ExprN, [ExprN])
collectArgs = second reverse . collectArgsR

argsToStr :: [ExprN] -> String
argsToStr =  intercalate ", " . map toGLSL

--------------------------------------

-- | A GLSL Variable.
data V a = V String (Type a)

instance Show (V a) where
    show (V n _) = "V " ++ n

instance HasGLSL (V a) where
    toGLSL (V name ty) = toGLSL ty ++ " " ++ name

instance HasType a => IsString (V a) where
    fromString name = V name (typeOf (undefined :: a))

-- | A pattern for a Variable.
type Pat a = Glom V a

pat :: forall a. HasType a => String -> Pat a
pat vname = dvy "" (typeOf (undefined :: a))
  where
    dvy :: HasType s => String -> Type s -> Pat s
    dvy _   UnitT      = UnitG
    dvy str (a :*:  b) = dvy (fstP str) a :* dvy (sndP str) b
    dvy _   (_ :->: _) = error "pat: Can't create a pattern for (:->:)."
    dvy str t          = BaseG $ V (namePath vname str) t

patV :: (HasType a, HasGLSL a) => V a -> Pat a
patV (V vname _) = pat vname

namePath :: String -> String -> String
namePath vname "" = vname
namePath vname p  = vname ++ "_" ++ p

fstP :: String -> String
fstP = ("fst"++)
sndP :: String -> String
sndP = ("snd"++)

-- | A GLSL Literal.
data Lit a where
    Literal :: HasGLSL a => a -> Lit a
    Native :: String -> Lit a
    BinOp :: String -> Lit a --(Type a)
    UnOp :: String -> Lit a --(Type a)
    PrimOp :: Type a -> Lit a
    Pair :: Lit (a -> b -> (a, b))
    FieldAccess :: String -> Lit a

instance Show (Lit a) where
    show (Literal l) = "Literal (" ++ toGLSL l ++ ")"
    show (Native  n) = "Native (" ++ n ++ ")"
    show (BinOp b) = "BinOp (" ++ b ++ ")"
    show (UnOp u) = "UnOp (" ++ u ++ ")"
    show (PrimOp{}) = "PrimOp"
    show Pair = "Pair"
    show (FieldAccess f) = "FieldAccess " ++ f

instance HasGLSL (Lit a) where
    toGLSL (Literal a) = toGLSL a
    toGLSL (Native s) = s
    toGLSL (BinOp s) = s
    toGLSL (UnOp s) = s
    toGLSL (FieldAccess f) = f
    toGLSL Pair = errorWithStackTrace "toGLSL Pair"
    toGLSL (PrimOp _) = errorWithStackTrace "toGLSL PrimOp"

---------------------------------------
-- Helpers to make Lam easier to use --
---------------------------------------

-- | A function that can be lifted into an
--   'Expr' via 'Lam'.
class Lambda a where
    type LamTy a :: *
    lam :: a -> Expr (LamTy a)

instance Lambda (Expr a) where
    type LamTy (Expr a) = a
    lam = id
instance Lambda b => Lambda (Expr a -> b) where
    type LamTy (Expr a -> b) = a -> LamTy b
    lam f = Lam $ \a -> lam (f a)

-----------------------
-- Reduction of Expr --
-----------------------

betaReduce :: Expr a -> Expr a
betaReduce (Lam f :$ x) = betaReduce $ f (betaReduce x)
betaReduce (f :$ x) = betaReduce' $ betaReduce f :$ betaReduce x
  where
    betaReduce' (Lam f' :$ x') = betaReduce $ f' (betaReduce x')
    betaReduce' (f' :$ x') = betaReduce f' :$ betaReduce x'
    betaReduce' x' = x'
betaReduce v@Var{} = v
betaReduce l@Lit{} = l
betaReduce l@Lam{} = l
betaReduce _ = errorWithStackTrace "betaReduce: Unknown arg."

-----------
-- Utils --
-----------

paren :: String -> String
paren str = "(" ++ str ++ ")"

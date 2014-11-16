{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where --Language.GLSL.Monad.AST where

import GHC.Stack
import Data.Word (Word)
import Data.Char (toLower)
import Data.Proxy
import GHC.TypeLits
import Control.Monad.Writer
import Data.List (intercalate)
import Debug.Trace (trace)
import Unsafe.Coerce (unsafeCoerce)
import Data.String (IsString(..))

import Data.Vec ((:.)(..), Vec2, Vec3, Vec4, Mat22, Mat33, Mat44)
import qualified Data.Vec as V

import qualified Graphics.Rendering.OpenGL.GL as GL

main :: IO ()
main = putStrLn $ toGLSL myShDef

scalar :: KnownScalar a => Scalar a -> String -> Expr a
scalar ty name = Var (V name (VectT (Vect (TS TZ) ty)))

scalarTy :: KnownScalar a => Scalar a -> Type a
scalarTy ty = VectT (Vect (TS TZ) ty)

pair :: forall a b. (GPU a, GPU b) => Expr (a -> b -> (a, b))
pair = --Lit Pair
    let ta = typeOf (undefined :: a)
        tb = typeOf (undefined :: b)
        tf = ta :->: tb :->: (ta :*: tb)
    in Lit (PrimOp tf)

unit :: Expr ()
unit = Lit (PrimOp UnitT)

myVert :: VertShader (Float, Int) Float
myVert = Lam $ \flInt ->
    let (flExp, iExp) = unPair flInt
    in flExp

myFrag :: FragShader (Float, Int) ()
myFrag = Lam $ \pExp ->
    let (flExp, iExp) = unPair pExp
        vec3 = Lit (Native "vec3")
        add = Lit (BinOp "+")
        pos = vec3 :# (add :# flExp :# iExp)
    in pair :# pos :# unit

fragAp :: Expr (Vec3 Float, ())
fragAp = myFrag :# (pair :# scalar SFloat "speed" :# scalar SInt "count")

myShDef :: Definition
myShDef =
    let myPat = pat "yolo"
--        vertArgTy = scalarTy SFloat :*: scalarTy SInt
--        vertArg = pair :# scalar SFloat "speed" :# scalar SInt "count"
--        vertApplied = myVert :# vertArg
    in Definition Nothing "main" (myPat =: fragAp)

-------------
-- Shaders --
-------------

type Shader i o = Expr (i -> o) -- or (Expr i -> Expr o)?
type ShaderPair i o = forall vo. (Shader i vo, Shader vo o)

type VertShader i o = Shader i o
type FragShader i o = Shader i (Vec3 Float, o)

---------------
-- Utilities --
---------------

-- | An OpenGL Sampler object, value-level.
data Sampler (n :: Nat) = Sampler (NatR n) GL.TextureObject

-- | Type-level Nat.
data NatR (n :: Nat) where
    TS :: NatR n -> NatR (n+1)
    TZ :: NatR 0

natRToInt :: NatR n -> Int
natRToInt TZ = 0
natRToInt (TS k) = 1 + natRToInt k

{-
-- | Type-level Nat.
type NatT = Proxy

tSucc :: NatT n -> NatT (n+1)
tSucc _ = Proxy
tZero :: NatT 0
tZero = Proxy
-}

------------------------
-- HasGLSL type class --
------------------------

class HasGLSL a where
    toGLSL :: a -> String

instance HasGLSL Int where
    toGLSL = show
instance HasGLSL Float where
    toGLSL = show
instance HasGLSL Word where
    toGLSL = show
instance HasGLSL Bool where
    toGLSL = map toLower . show

instance HasGLSL () where
    toGLSL _ = "()"

instance (HasGLSL a, HasGLSL b) => HasGLSL (a, b) where
    toGLSL (l, r) = "(" ++ toGLSL l ++ ", " ++ toGLSL r ++ ")"

instance HasGLSL a => HasGLSL (Vec2 a) where
    toGLSL (x:.y:.()) =
        "vec2(" ++ toGLSL x ++ ", " ++
                   toGLSL y ++ ")"
instance HasGLSL a => HasGLSL (Vec3 a) where
    toGLSL (x:.y:.z:.()) =
        "vec3(" ++ toGLSL x ++ ", " ++
                   toGLSL y ++
                   toGLSL z ++ ")"
instance HasGLSL a => HasGLSL (Vec4 a) where
    toGLSL (x:.y:.z:.w:.()) =
        "vec4(" ++ toGLSL x ++ ", " ++
                   toGLSL y ++
                   toGLSL z ++
                   toGLSL w ++ ")"

instance HasGLSL (a -> b) where
    toGLSL = errorWithStackTrace $
        "toGLSL (a -> b): There isn't actually a way to do this."

-----------------
-- Type system --
-----------------

-- | Type and value-level representation
--   of GLSL Types.
data Type a where
    VectT :: (KnownNat n, KnownScalar a) => Vect n a -> Type (VecN n a)
    MatT :: KnownNat n => Mat  n -> Type (MatN n)
    SamplerT :: KnownNat n => NatR n -> Type (Sampler n)
    UnitT :: Type ()
    (:*:) :: (GPU a, GPU b, HasGLSL a, HasGLSL b) =>
              Type a -> Type b -> Type (a, b)
    (:->:) :: (GPU a, GPU b) =>
               Type a -> Type b -> Type (a -> b)
infixr 5 :->:

instance Eq (Type a) where
    VectT _     == VectT _     = True
    MatT  _     == MatT  _     = True
    SamplerT _  == SamplerT _  = True
    UnitT       == UnitT       = True
    (a1 :*: b1) == (a2 :*: b2) = a1 == a2 && b1 == b2
    (a1:->: b1) == (a2:->: b2) = a1 == a2 && b1 == b2
    _           == _           = False

instance HasGLSL (Type a) where
    toGLSL (VectT vect) = toGLSL vect
    toGLSL (MatT  mat)  = toGLSL mat
    toGLSL (SamplerT nat) =
        "sampler" ++ show (natRToInt nat) ++ "D"
    toGLSL UnitT =
        error $ "toGLSL Type: UnitT does not have " ++
                "a GLSL representation."
    toGLSL (_ :*:  _) =
        error $ "toGLSL Type: (:*:) does not have " ++
                "a GLSL representation."
    toGLSL (_ :->: _) =
        error $ "toGLSL Type: (:->:) does not have " ++
                "a GLSL representation."

data Scalar a where
    SInt :: Scalar Int
    SUInt :: Scalar Word
    SFloat :: Scalar Float
    SBool :: Scalar Bool

instance HasGLSL (Scalar a) where
    toGLSL SInt = "int"
    toGLSL SUInt = "uint"
    toGLSL SFloat = "float"
    toGLSL SBool = "bool"

vecPrefix :: Scalar a -> String
vecPrefix SInt = "i"
vecPrefix SUInt = "u"
vecPrefix SFloat = ""
vecPrefix SBool = "b"

data Vect (n :: Nat) a where
    Vect :: NatR n -> Scalar a -> Vect n a

instance HasGLSL (Vect n a) where
    toGLSL (Vect (TS TZ) scal) =
        toGLSL scal
    toGLSL (Vect nat scal) =
        vecPrefix scal ++ "vec" ++
        show (natRToInt nat)

-- | Type of a square matrix.
data Mat (n :: Nat) where
    Mat :: NatR n -> Mat n

instance HasGLSL (Mat n) where
    toGLSL (Mat (TS TZ)) = toGLSL SFloat
    toGLSL (Mat nat) = "mat" ++ times 2 (show $ natRToInt nat)
      where
        times :: Int -> [a] -> [a]
        times i xs | i < 1     = []
                   | i == 1    = xs
                   | otherwise = xs ++ times (i-1) xs

-- | 'Data.Vec' Vec*s indexed over 'GHC.TypeLits.Nat'.
type family VecN (n :: Nat) (a :: *) :: * where
    VecN 1 a = a
    VecN 2 a = Vec2 a
    VecN 3 a = Vec3 a
    VecN 4 a = Vec4 a

-- | 'Data.Vec' Mat*s indexed over 'GHC.TypeLits.Nat'.
type family MatN (n :: Nat) :: * where
    MatN 1 = Float
    MatN 2 = Mat22 Float
    MatN 3 = Mat33 Float
    MatN 4 = Mat44 Float

------------------
-- GPU / Scalar --
------------------

-- | Anything that can be used on the GPU.
class HasGLSL a => GPU a where
    typeOf :: a -> Type a

-- | Types whose GLSL counterparts are scalars.
class GPU a => KnownScalar a where
    scalarType :: a -> Scalar a


instance GPU () where
    typeOf _ = UnitT
-- Scalars
instance GPU Int where
    typeOf _ = VectT (Vect (TS TZ) SInt)
instance GPU Word where
    typeOf _ = VectT (Vect (TS TZ) SUInt)
instance GPU Float where
    typeOf _ = VectT (Vect (TS TZ) SFloat)
instance GPU Bool where
    typeOf _ = VectT (Vect (TS TZ) SBool)
-- Function type
instance (GPU a, GPU b) => GPU (a -> b) where
    typeOf _ = typeOf (undefined :: a) :->: typeOf (undefined :: b)
-- Tuples
instance (GPU a, GPU b, HasGLSL a, HasGLSL b) => GPU (a, b) where
    typeOf _ = typeOf (undefined :: a) :*: typeOf (undefined :: b)
-- Vecs
instance KnownScalar a => GPU (Vec2 a) where
    typeOf _ =
        let len = TS (TS TZ)
            scalarT = scalarType (undefined :: a)
        in VectT (Vect len scalarT)
instance KnownScalar a => GPU (Vec3 a) where
    typeOf _ =
        let len = TS (TS (TS TZ))
            scalarT = scalarType (undefined :: a)
        in VectT (Vect len scalarT)
instance KnownScalar a => GPU (Vec4 a) where
    typeOf _ =
        let len = TS (TS (TS (TS TZ)))
            scalarT = scalarType (undefined :: a)
        in VectT (Vect len scalarT)
-- Matrices
instance GPU (Mat22 Float) where
    typeOf _ =
        let len = TS (TS TZ)
        in MatT (Mat len)
instance GPU (Mat33 Float) where
    typeOf _ =
        let len = TS (TS (TS TZ))
        in MatT (Mat len)
instance GPU (Mat44 Float) where
    typeOf _ =
        let len = TS (TS (TS (TS TZ)))
        in MatT (Mat len)

instance KnownScalar Int where
    scalarType _ = SInt
instance KnownScalar Word where
    scalarType _ = SUInt
instance KnownScalar Float where
    scalarType _ = SFloat
instance KnownScalar Bool where
    scalarType _ = SBool

-----------------------------------------------
-- Functional lambda calculus-based frontend --
-----------------------------------------------

-- | Lambda calculus with HOAS.
data Expr a where
    Var :: V a -> Expr a
    Lit :: Lit a -> Expr a
    Vam :: V a -> Expr b -> Expr (a -> b)
    Lam :: (Expr a -> Expr b) -> Expr (a -> b)
    (:#) :: Expr (a -> b) -> Expr a -> Expr b
infixl 9 :#

instance Show (Expr a) where
    show (Var v) = "Var (" ++ show v ++ ")"
    show (Lit l) = "Lit (" ++ show l ++ ")"
    show (Lam _) = "Lam (....)"
    show (a:# b) = "(" ++ show a ++ ") :# (" ++ show b ++ ")"
    show (Vam v e) = "Vam (" ++ show v ++ ") (" ++ show e ++ ")"

instance (Num a, HasGLSL a, GPU a) => Num (Expr a) where
    fromInteger int =
        Lit . BinOp . toGLSL $ (fromInteger int :: a)
    (+) a b = Lit (BinOp "+") :# a :# b
    (-) a b = Lit (BinOp "-") :# a :# b
    (*) a b = Lit (BinOp "*") :# a :# b
    abs x = Lit (Native "abs") :# x
    signum = error "Called signum on 'Expr a'."

instance HasGLSL (Expr a) where
    toGLSL (Var (V n _)) = n
    toGLSL (Lit lit) = toGLSL lit
    toGLSL (Vam v e) = toGLSL v ++ " = " ++ toGLSL e
    toGLSL (Lam f :# x) = toGLSL $ f x
    toGLSL app@(_ :# _) =
        let (func, args) = collectArgs app
            argsStr = argsToStr args
        in case func of
            ExprN (Lit lit) ->
                case lit of
                    UnOp f  -> f ++ assertArgLen args 1 argsStr
                    BinOp f -> assertArgLen args 2 $
                        toGLSL (head args) ++ f ++ toGLSL (last args)
                    f       -> toGLSL (Lit f) ++ "(" ++ argsStr ++ ")"
            ExprN (Var (V n t)) -> toGLSL (Var (V n t))
            ExprN _ -> errorWithStackTrace $
                "Error in toGLSL Expr: case didn't match on" ++
                "Lit or Var, and the other cases are impossible!"
      where
        assertArgLen args len x
            | length args /= len =
                errorWithStackTrace "Error in 'assertArgLen'"
            | otherwise = x
    toGLSL (Lam _) =
        errorWithStackTrace "Error in toGLSL Expr: recieved unapplied Lam."

-- Helpers for HasGLSL Expr instance --

-- | Existential Expr.
data ExprN = forall a. ExprN (Expr a)
instance HasGLSL ExprN where
    toGLSL (ExprN e) = toGLSL e

collectArgsR :: Expr a -> (ExprN, [ExprN])
collectArgsR (xs :# x) =
    let (f,as) = collectArgsR xs
    in (f, ExprN x : as)
collectArgsR x = (ExprN x,[])

argList :: Expr a -> String
argList = argsToStr . snd . collectArgs

collectArgs :: Expr a -> (ExprN, [ExprN])
collectArgs = (\(f,xs) -> (f, reverse xs)) . collectArgsR

argsToStr :: [ExprN] -> String
argsToStr =  intercalate ", " . map toGLSL

---------

-- | A GLSL Variable.
data V a = V String (Type a)

instance Show (V a) where
    show (V n _) = "V " ++ n

instance HasGLSL (V a) where
    toGLSL (V name ty) = toGLSL ty ++ " " ++ name

instance GPU a => IsString (V a) where
    fromString name = V name (typeOf (undefined :: a))

-- | A GLSL Literal.
data Lit a where
    Literal :: ({-TODO GPU a, -} HasGLSL a) => a -> Lit a
    Native :: String -> Lit a
    BinOp :: String -> Lit a --(Type a)
    UnOp :: String -> Lit a --(Type a)
    PrimOp :: Type a -> Lit a
    Pair :: Lit (a -> b -> (a, b))

instance Show (Lit a) where
    show (Literal l) = "Literal (" ++ toGLSL l ++ ")"
    show (Native  n) = "Native (" ++ n ++ ")"
    show (BinOp b) = "BinOp (" ++ b ++ ")"
    show (UnOp u) = "UnOp (" ++ u ++ ")"
    show (PrimOp{}) = "PrimOp"
    show Pair = "Pair"

instance HasGLSL (Lit a) where
    toGLSL (Literal a) = toGLSL a
    toGLSL (Native s) = s
    toGLSL (BinOp s) = s
    toGLSL (UnOp s) = s
    toGLSL Pair = errorWithStackTrace "toGLSL Pair"
    toGLSL (PrimOp _) = errorWithStackTrace "toGLSL PrimOp"




data Glom f a where
    UnitG  :: Glom f ()
    BaseG :: (GPU a, HasGLSL a) => f a -> Glom f a
    (:*)  :: (GPU a, GPU b, HasGLSL a, HasGLSL b) => Glom f a -> Glom f b -> Glom f (a, b)
infixr 7 :*

type Pat a = Glom V a

pat :: forall a. (GPU a, HasGLSL a) => String -> Pat a
pat vname = dvy "" (typeOf (undefined :: a))
  where
    dvy :: (GPU s, HasGLSL s) => String -> Type s -> Pat s
    dvy _   UnitT      = UnitG
    dvy str (a :*:  b) = dvy (fstP str) a :* dvy (sndP str) b
    dvy _   (_ :->: _) = error "pat: Can't create a pattern for (:->:)."
    dvy str t          = BaseG $ V (namePath vname str) t

namePath :: String -> String -> String
namePath vname "" = vname
namePath vname p  = vname ++ "_" ++ p

fstP :: String -> String
fstP = ("fst"++)
sndP :: String -> String
sndP = ("snd"++)

data (:~:) a b where
    Refl :: a :~: a

{-
pattern PairFun t1 t2 tf ts a b = (Lit (PrimOp
        (VectT (Vect (TS TZ) t1) :->:
        (VectT (Vect (TS TZ) t2) :->:
        (VectT (Vect (TS TZ) tf) :*: VectT (Vect (TS TZ) ts)
        )))) :# a :# b)
-}

pattern PairFun t1 t2 tf ts a b = (Lit (PrimOp (
        t1 :->:
        t2 :->:
        (tf :*: ts)
        )) :# a :# b)

-- TODO: Completely rewrite Pair system, it's awkward.
unPair :: forall a b. (HasGLSL a, HasGLSL b) =>
    Expr (a, b) -> (Expr a, Expr b)
unPair (Lit Pair :# a :# b) = (a, b)
unPair (PairFun (_ :: Type aat) (_ :: Type abt)
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
        else errorWithStackTrace $ "unPair: Types are not all equal."
unPair (Lam f :# x) = unPair $ f x
unPair _ = errorWithStackTrace $ "unPair: Pair was through some " ++
                                 "unknown operation."

-----------------------
-- Reduction of Expr --
-----------------------

{-
betaReduce :: Expr a -> Expr a
betaReduce (Lam f :# x) = betaReduce $ f x
betaReduce v@Var{} = v
betaReduce l@Lit{} = l
-}

----------------------------------
-- Backend for functional front --
----------------------------------

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
(=:) :: (GPU a, HasGLSL a) => Pat a -> Expr a -> Statement
(=:) p e = Assign (Bind p e)

{-
-- Imperative backend (or frontend)

data GLSL a = GLSL {
    unGLSL :: Writer Statement a
    }

data ExprLog = ExprLog -- ...

data ExprImp (is :: [i]) a =
    ExprImp -- Writer ()

data Qualifier = In | Out | Uniform | NoQualifier

data Value (q :: Qualifier) (t :: *) where
  Value :: String -> Type t -> Value q t

data Expression (t :: *) where
    Expression :: String -> Expression t
-}

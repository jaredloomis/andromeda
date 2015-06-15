{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Andromeda.Simple.StdLib where

import GHC.TypeLits
import Data.Proxy
import Data.Vec (Vec2, Vec3, Vec4)

import Andromeda.Simple.Expr
import Andromeda.Simple.Type
import Andromeda.Simple.Util

--type (&-) a b = (a, b)
--infixl &-

---------------------------------------
-- Helpers to make Lam easier to use --
---------------------------------------

-- | A function that can be lifted into an
--   'Expr' via 'Lam'
class Lambda a where
    type LamTy a :: *
    lam :: a -> Expr (LamTy a)

instance Lambda (Expr a) where
    type LamTy (Expr a) = a
    lam = id
instance Lambda b => Lambda (Expr a -> b) where
    type LamTy (Expr a -> b) = a -> LamTy b
    lam f = Lam $ \a -> lam (f a)

lamp2 :: (Typed a, Typed b) => Expr (a -> b -> c) -> Expr ((a,b) -> c)
lamp2 f = Lam $ \p ->
    let (a,b) = unPair p
    in f :$ a :$ b

lamp3 :: (Typed a, Typed b, Typed c) =>
    Expr (a -> b -> c -> d) -> Expr ((a,(b,c)) -> d)
lamp3 f = Lam $ \p ->
    let (a,p') = unPair p
    in lamp2 (f :$ a) :$ p'

lamp4 :: (Typed a, Typed b, Typed c, Typed d) =>
    Expr (a -> b -> c -> d -> e) -> Expr ((a,(b,(c,d))) -> e)
lamp4 f = Lam $ \p ->
    let (a,p') = unPair p
    in lamp3 (f :$ a) :$ p'

lamp5 :: (Typed a, Typed b, Typed c, Typed d, Typed e) =>
    Expr (a -> b -> c -> d -> e -> f) -> Expr ((a,(b,(c,(d,e)))) -> f)
lamp5 f = Lam $ \p ->
    let (a,p') = unPair p
    in lamp4 (f :$ a) :$ p'
{-
class Lamp a where
    type Args a
    type Result a
    lamp :: Expr a -> Expr (Args a -> Result a)

instance (Typed a, Typed b, Lamp c,
          Typed (Args c)) => Lamp (a -> b -> c) where
    type Args (a -> b -> c) = (a, (b, Args c))
    type Result (a -> b -> c) = Result c
    lamp f = Lam $ \p ->
        let (a, p') = unPair p
            (b, argsc) = unPair p'
            c = f :$ a :$ b
            lc = lamp c
        in lc :$ argsc

instance Lamp (a -> Int) where
    type Args (a -> Int) = a
    type Result (a -> Int) = Int
    lamp = id

instance Lamp Int where
    type Args Int = ()
    type Result Int = Int
    lamp i = Lam $ const i
instance Lamp Float where
    type Args Float = ()
    type Result Float = Float
    lamp i = Lam $ const i
instance Lamp Word where
    type Args Word = ()
    type Result Word = Word
    lamp i = Lam $ const i
instance Lamp Bool where
    type Args Bool = ()
    type Result Bool = Bool
    lamp i = Lam $ const i
-}

-------------------
-- Vec appending --
-------------------

-- | 'VecLength' is a typeclass that allows us to
--   associate a scalar, vector, and a length with
--   each other.
class VecLength s (v :: *) (n :: Nat) | v -> n s, n s -> v,
                                        v n -> s, v s -> n

instance VecLength a (Vec2 a) 2
instance VecLength a (Vec3 a) 3
instance VecLength a (Vec4 a) 4
instance VecLength Float Float 1
instance VecLength Int   Int   1
instance VecLength Bool  Bool  1
instance VecLength Word  Word  1

(+-+) :: forall a b c s n1 n2.
        (VecLength s a n1, VecLength s b n2,
         VecLength s c (n1+n2), KnownNat n1, KnownNat n2) =>
    Expr a -> Expr b -> Expr c
(+-+) x y =
    let len = natVal (Proxy :: Proxy n1) + natVal (Proxy :: Proxy n2)
        funcName = "vec" ++ show len
    in Lit (Native funcName) :$ x :$ y
infixr 5 +-+

-------------------
-- Vec swizzling --
-------------------

data Index (len :: Nat) (maxI :: Nat) where
    X :: Index 1 1
    Y :: Index 1 2
    Z :: Index 1 3
    W :: Index 1 4
    Linked :: Index len1 max1 -> Index len2 max2 ->
              Index (len1 + len2) (Max max1 max2)

(&) :: Index len1 max1 -> Index len2 max2 ->
        Index (len1 + len2) (Max max1 max2)
(&) = Linked

instance Show (Index len s) where
    show X = "x"
    show Y = "y"
    show Z = "z"
    show W = "w"
    show (Linked list1 list2) = show list1 ++ show list2

(!) :: (VecLength s a n, VecLength s b len, maxI <= n) =>
    Expr a -> Index len maxI -> Expr b
(!) vec index = Lit (FieldAccess $ show index) :$ vec
infixl 8 !

-- Helpers

type family If (condition :: Bool) (yes :: Nat) (no :: Nat) :: Nat where
    If 'True  yes no = yes
    If 'False yes no = no

type Max a b = If (a <=? b) b a

----------------
-- Matrix ops --
----------------

class MatrixMult a b c | a b -> c, c b -> a, c a -> b where

instance MatrixMult (Matrix2 Float) (Vec2 Float) (Vec2 Float)
instance MatrixMult (Matrix3 Float) (Vec3 Float) (Vec3 Float)
instance MatrixMult (Matrix4 Float) (Vec4 Float) (Vec4 Float)

(#*) :: MatrixMult a b c => Expr a -> Expr b -> Expr c
(#*) mat vec = Lit (BinOp "*") :$ mat :$ vec

---------------
-- Functions --
---------------

type family Texture (a :: *) :: * where
    Texture Sampler1D =      Float
    Texture Sampler2D = Vec2 Float
    Texture Sampler3D = Vec3 Float

texture :: Expr a -> Expr (Texture a) -> Expr (Vec4 Float)
texture img vec = Lit (Native "texture") :$ img :$ vec

floorG :: RealFrac a => Expr a -> Expr a
floorG x = Lit (Native "floor") :$ x

----------------
-- Misc utils --
----------------

fetch :: String -> Type a -> Expr a
fetch attr ty = Lit $ Fetch attr ty

uniform :: String -> Type a -> Expr a
uniform unif ty = Lit $ Unif unif ty

pair :: (Typed a, Typed b) => Expr a -> Expr b -> Expr (a, b)
pair x y = Lit Pair :$ x :$ y

bottom :: Expr ()
bottom = Lit (Native "__BOTTOM__")

flt :: Expr Float -> Expr Float
flt = id

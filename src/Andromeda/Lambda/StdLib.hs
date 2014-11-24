{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
module Andromeda.Lambda.StdLib where

import GHC.TypeLits
import Data.Proxy (Proxy(..))
import Data.Word (Word)

import Data.Vec (Vec2, Vec3, Vec4)
import qualified Data.Vec as Vec

import Andromeda.Lambda.Expr
import Andromeda.Lambda.Type

pair :: forall a b. (HasType a, HasType b) => Expr (a -> b -> (a, b))
pair = -- Lit Pair
    let ta = typeOf (undefined :: a)
        tb = typeOf (undefined :: b)
        tf = ta :->: tb :->: (ta :*: tb)
    in Lit (PrimOp tf)

-------------------
-- Vec appending --
-------------------

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

-------------------
-- Vec swizzling --
-------------------

data Index (len :: Nat) (maxI :: Nat) where
    X :: Index 1 1
    Y :: Index 1 2
    Z :: Index 1 3
    W :: Index 1 4
    Linked :: Index len1 max1 -> Index len2 max2 ->
              Index (len1 + len2) (If (max1 <=? max2) max2 max1)

(&) :: Index len1 max1 -> Index len2 max2 ->
        Index (len1 + len2) (If (max1 <=? max2) max2 max1)
(&) = Linked

type family If (condition :: Bool) (yes :: Nat) (no :: Nat) :: Nat where
    If True  yes no = yes
    If False yes no = no

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

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

pair :: (HasType a, HasType b) => Expr (a -> b -> (a, b))
pair = Lit Pair

-- Synonyms for 'lam' --

--inn :: Lambda a => a -> Expr (LamTy a)
--inn = lam

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
    If True  yes no = yes
    If False yes no = no

type Max a b = If (a <=? b) b a

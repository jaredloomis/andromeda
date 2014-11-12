{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Language.GLSL.Monad.Vec where

import Data.Monoid ((<>))
import Data.String (fromString)
import Data.Proxy
import GHC.TypeLits

import Language.GLSL.Monad.Type
import Language.GLSL.Monad.GLSL
import Language.GLSL.Monad.GPU

--------------------
-- Vec conversion --
--------------------

type family VecLength (v :: Type) :: Nat where
    VecLength GFloat = 1
    VecLength GVec2  = 2
    VecLength GVec3  = 3
    VecLength GVec4  = 4

type family VecLengthU (n :: Nat) :: Type where
    VecLengthU 1 = GFloat
    VecLengthU 2 = GVec2
    VecLengthU 3 = GVec3
    VecLengthU 4 = GVec4

-- = Vec "concatenation"

vConcat :: forall a at b bt ct.
           (HasGPUCode a, ReadableQ a, TypeOf a ~ at,
            HasGPUCode b, ReadableQ b, TypeOf b ~ bt,
            VecLengthU (VecLength at + VecLength bt) ~ ct,
            Reify ct Type) =>
     a -> b -> Expression ct
vConcat left right =
    let resultProxy = Proxy :: Proxy ct
        resultCtor = show (reify resultProxy :: Type)
    in Expression (paren $ fromString resultCtor <>
        paren (getGPUCode left <> ", " <> getGPUCode right))
        Proxy

(+.+) :: forall a at b bt ct.
           (HasGPUCode a, ReadableQ a, TypeOf a ~ at,
            HasGPUCode b, ReadableQ b, TypeOf b ~ bt,
            VecLengthU (VecLength at + VecLength bt) ~ ct,
            Reify ct Type) =>
     a -> b -> Expression ct
(+.+) = vConcat
infixr 5 +.+

-- = Vec swizzling

data Index (len :: Nat) (maxI :: Nat) where
    X :: Index 1 1
    Y :: Index 1 2
    Z :: Index 1 3
    W :: Index 1 4
    Linked :: Index len1 max1 -> Index len2 max2 ->
              Index (len1 + len2) (If (max1 <=? max2) max2 max1)

(.&) :: Index len1 max1 -> Index len2 max2 ->
        Index (len1 + len2) (If (max1 <=? max2) max2 max1)
(.&) = Linked

type family If (condition :: Bool) (yes :: Nat) (no :: Nat) :: Nat where
    If True yes no = yes
    If False yes no = no

instance BShow (Index len s) where
    bshow X = "x"
    bshow Y = "y"
    bshow Z = "z"
    bshow W = "w"
    bshow (Linked list1 list2) = bshow list1 <> bshow list2

(.@) :: forall a maxI len.
        (HasGPUCode a, ReadableQ a,
         maxI <= VecLength (TypeOf a)) =>
    a -> Index len maxI -> Expression (VecLengthU len)
(.@) vec index =
    let swizzleStr = bshow index
    in Expression (paren $ getGPUCode vec <> "." <> swizzleStr)
        Proxy
infixl 8 .@

--------------------
-- Array indexing --
--------------------

class IndexT t
instance IndexT Int
instance IndexT Integer
instance IndexT (Expression GInt)
instance IndexT (Value q GInt)
instance IndexT (Expression GUInt)
instance IndexT (Value q GUInt)

(.!) :: (ReadableQ a, HasGPUCode a, IndexT a) =>
    Array q t -> a -> Expression t
(.!) (Array name _ _) i =
    Expression (name <> "[" <> getGPUCode i <> "]") Proxy

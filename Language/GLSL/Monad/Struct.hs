{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
module Language.GLSL.Monad.Struct where

import Data.Proxy

import Language.GLSL.Monad.Type
import Language.GLSL.Monad.GPU

{-
-- EXAMPLE
data Vert = Vert Float Float Float
instance Struct Vert where
    type GPUStruct Vert = Pair (Pair 'Float 'Float) 'Float
    type Unpacked Vert = ((Float, Float), Float)
    unpackStruct (Vert a b c) = ((a,b),c)
--------
-}
class Struct a where
    type GPUStruct a :: Type
    type Unpacked a :: *
    unpackStruct :: a -> Unpacked a

{-
struct :: (Struct str, GPUStruct str ~ gpuS, GPU gpuS,
           Reify gpuS Type, Reify q Qualifier) =>
    [B.ByteString] -> Proxy q -> Proxy t ->
    B.ByteString -> ShaderM s gt (Value q gpuS)
struct layouts qualifier glType name = do
    undefined
--    layoutDecl layouts qualifier glType name
-}

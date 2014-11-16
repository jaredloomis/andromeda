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
    type Unpacked Vert = ((Float, Float), Float)
    unpackStruct (Vert a b c) = ((a,b),c)
--------
-}
class Struct a where
    type Unpacked a :: *
    unpackStruct :: a -> Unpacked a

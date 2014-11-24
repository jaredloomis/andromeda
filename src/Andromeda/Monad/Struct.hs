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
module Andromeda.Monad.Struct where

import Data.Proxy

import Andromeda.Monad.Type
import Andromeda.Monad.GPU

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

{-# LANGUAGE RankNTypes #-}
module Shader where

import Data.Vec (Vec3)

import Expr

type Shader i o = Expr (i -> o) -- or (Expr i -> Expr o)?
type ShaderPair i o = forall vo. (Shader i vo, Shader vo o)

type VertShader i o = Shader i (Vec3 Float, o)
type FragShader i o = Shader i (Vec3 Float, o)

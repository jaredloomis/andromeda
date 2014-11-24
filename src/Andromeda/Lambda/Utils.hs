{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module Andromeda.Lambda.Utils where

import GHC.TypeLits

import qualified Graphics.Rendering.OpenGL.GL as GL

import Andromeda.Lambda.NatR

-- | An OpenGL Sampler object, value-level.
data Sampler (n :: Nat) = Sampler (NatR n) GL.TextureObject

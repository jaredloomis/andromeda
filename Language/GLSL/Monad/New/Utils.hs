{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module Utils where

import GHC.TypeLits

import qualified Graphics.Rendering.OpenGL.GL as GL

import NatR

-- | An OpenGL Sampler object, value-level.
data Sampler (n :: Nat) = Sampler (NatR n) GL.TextureObject

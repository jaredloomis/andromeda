{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
module Andromeda.Lambda.Utils where

import GHC.TypeLits
import Data.Vec (Mat22,Mat33,Mat44)

import qualified Graphics.Rendering.OpenGL.GL as GL

import Andromeda.Lambda.NatR

-- | An OpenGL Sampler object, term-level.
data Sampler (n :: Nat) = Sampler (NatR n) GL.TextureObject

data family Matrix (n :: Nat) :: *
data instance Matrix 2 = Mat2 (Mat22 Float)
data instance Matrix 3 = Mat3 (Mat33 Float)
data instance Matrix 4 = Mat4 (Mat44 Float)

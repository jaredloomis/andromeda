module Andromeda.Simple.Util where

import Data.Vec (Mat22, Mat33, Mat44)

import qualified Graphics.Rendering.OpenGL.GL as GL

----------------------------------------
-- Haskell representation of samplers --
----------------------------------------

newtype Sampler1D = Sampler1D GL.TextureObject
newtype Sampler2D = Sampler2D GL.TextureObject
newtype Sampler3D = Sampler3D GL.TextureObject

-----------------------------------------------------------
-- Matrix newtypes to avoid conflicts with vec instances --
-----------------------------------------------------------

newtype Matrix2 a = Matrix2 (Mat22 a)
newtype Matrix3 a = Matrix3 (Mat33 a)
newtype Matrix4 a = Matrix4 (Mat44 a)

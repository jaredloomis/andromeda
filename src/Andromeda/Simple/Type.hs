{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
module Andromeda.Simple.Type where

import Data.Vec (Vec2, Vec3, Vec4)

import qualified Graphics.Rendering.OpenGL.GL as GL

import Andromeda.Simple.GLSL
import Andromeda.Simple.Util

-- | Type and term-level Haskell
--   representation of GLSL Types
data Type a where
    ScalarT    :: KnownScalar a => Scalar a -> Type a
    Vec2T      :: KnownScalar a => Scalar a -> Type (Vec2 a)
    Vec3T      :: KnownScalar a => Scalar a -> Type (Vec3 a)
    Vec4T      :: KnownScalar a => Scalar a -> Type (Vec4 a)
    Matrix2T   :: KnownScalar a => Scalar a -> Type (Matrix2 a)
    Matrix3T   :: KnownScalar a => Scalar a -> Type (Matrix3 a)
    Matrix4T   :: KnownScalar a => Scalar a -> Type (Matrix4 a)
    Sampler1DT ::                              Type Sampler1D
    Sampler2DT ::                              Type Sampler2D
    Sampler3DT ::                              Type Sampler3D
    UnitT      ::                              Type ()
    (:*:)      :: (Typed a, Typed b, GLSL a, GLSL b) =>
                   Type a -> Type b -> Type (a, b)
    (:->:)     :: (Typed a, Typed b) =>
                   Type a -> Type b -> Type (a -> b)
infixr 5 :->:

instance GLSL (Type a) where
    toGLSL (ScalarT scalar) = toGLSL scalar
    toGLSL (Vec2T ty)       = vecPrefix ty ++ "vec2"
    toGLSL (Vec3T ty)       = vecPrefix ty ++ "vec3"
    toGLSL (Vec4T ty)       = vecPrefix ty ++ "vec4"
    toGLSL (Matrix2T ty)    = vecPrefix ty ++ "mat2"
    toGLSL (Matrix3T ty)    = vecPrefix ty ++ "mat3"
    toGLSL (Matrix4T ty)    = vecPrefix ty ++ "mat4"
    toGLSL Sampler1DT       = "sampler1D"
    toGLSL Sampler2DT       = "sampler2D"
    toGLSL Sampler3DT       = "sampler3D"
    toGLSL UnitT =
        error $ "toGLSL Type: UnitT does not have " ++
                "a GLSL representation."
    toGLSL (_ :*:  _) =
        error $ "toGLSL Type: (:*:) does not have " ++
                "a GLSL representation."
    toGLSL (_ :->: _) =
        error $ "toGLSL Type: (:->:) does not have " ++
                "a GLSL representation."

data Scalar a where
    SInt   :: Scalar Int
    SUInt  :: Scalar Word
    SFloat :: Scalar Float

instance GLSL (Scalar a) where
    toGLSL SInt   = "int"
    toGLSL SUInt  = "uint"
    toGLSL SFloat = "float"

vecPrefix :: Scalar a -> String
vecPrefix SInt   = "i"
vecPrefix SUInt  = "u"
vecPrefix SFloat = ""

-------------------------
-- Typed / KnownScalar --
-------------------------

-- | Anything that has a GLSL Type.
class GLSL a => Typed a where
    typeOf :: a -> Type a

-- | Types whose GLSL counterparts are scalars.
class Typed a => KnownScalar a where
    scalarType :: a -> Scalar a
    glScalarType :: a -> GL.DataType

instance Typed () where
    typeOf _ = UnitT
-- Scalars
instance Typed Int where
    typeOf = ScalarT . scalarType
instance Typed Word where
    typeOf = ScalarT . scalarType
instance Typed Float where
    typeOf = ScalarT . scalarType
-- Function type
instance (Typed a, Typed b) => Typed (a -> b) where
    typeOf _ = typeOf (undefined :: a) :->: typeOf (undefined :: b)
-- Tuples
instance (Typed a, Typed b, GLSL a, GLSL b) => Typed (a, b) where
    typeOf _ = typeOf (undefined :: a) :*: typeOf (undefined :: b)
-- Vecs
instance KnownScalar a => Typed (Vec2 a) where
    typeOf _ = Vec2T $ scalarType (undefined :: a)
instance KnownScalar a => Typed (Vec3 a) where
    typeOf _ = Vec3T $ scalarType (undefined :: a)
instance KnownScalar a => Typed (Vec4 a) where
    typeOf _ = Vec4T $ scalarType (undefined :: a)
-- Matrices
instance KnownScalar a => Typed (Matrix2 a) where
    typeOf _ = Matrix2T $ scalarType (undefined :: a)
instance KnownScalar a => Typed (Matrix3 a) where
    typeOf _ = Matrix3T $ scalarType (undefined :: a)
instance KnownScalar a => Typed (Matrix4 a) where
    typeOf _ = Matrix4T $ scalarType (undefined :: a)
-- Samplers
instance Typed Sampler1D where
    typeOf _ = Sampler1DT
instance Typed Sampler2D where
    typeOf _ = Sampler2DT
instance Typed Sampler3D where
    typeOf _ = Sampler3DT

-- Known Scalars
instance KnownScalar Int where
    scalarType _ = SInt
    glScalarType _ = GL.Int
instance KnownScalar Word where
    scalarType _ = SUInt
    glScalarType _ = GL.UnsignedInt
instance KnownScalar Float where
    scalarType _ = SFloat
    glScalarType _ = GL.Float

-----------
-- Utils --
-----------

guessTy :: Typed a => Type a
guessTy = typeOf undefined

isPair :: Type a -> Bool
isPair (_ :*: _) = True
isPair _         = False

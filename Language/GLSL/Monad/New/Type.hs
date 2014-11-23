{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
module Type where

import GHC.TypeLits
import Data.Word (Word)
import Data.Vec (Vec2,Vec3,Vec4,Mat22,Mat33,Mat44)

import qualified Graphics.Rendering.OpenGL.GL as GL

import HasGLSL
import NatR
import Utils

-- | Type and value-level representation
--   of GLSL Types.
data Type a where
    VectT :: (KnownNat n, KnownScalar a) => Vect n a -> Type (VecN n a)
    MatT :: KnownNat n => Mat  n -> Type (MatN n)
    SamplerT :: KnownNat n => NatR n -> Type (Sampler n)
    UnitT :: Type ()
    (:*:) :: (HasType a, HasType b, HasGLSL a, HasGLSL b) =>
              Type a -> Type b -> Type (a, b)
    (:->:) :: (HasType a, HasType b) =>
               Type a -> Type b -> Type (a -> b)
infixr 5 :->:

instance Eq (Type a) where
    VectT _     == VectT _     = True
    MatT  _     == MatT  _     = True
    SamplerT _  == SamplerT _  = True
    UnitT       == UnitT       = True
    (a1 :*: b1) == (a2 :*: b2) = a1 == a2 && b1 == b2
    (a1:->: b1) == (a2:->: b2) = a1 == a2 && b1 == b2
    _           == _           = False

instance HasGLSL (Type a) where
    toGLSL (VectT vect) = toGLSL vect
    toGLSL (MatT  mat)  = toGLSL mat
    toGLSL (SamplerT nat) =
        "sampler" ++ show (natRToInt nat) ++ "D"
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
    SInt :: Scalar Int
    SUInt :: Scalar Word
    SFloat :: Scalar Float
    SBool :: Scalar Bool

instance HasGLSL (Scalar a) where
    toGLSL SInt = "int"
    toGLSL SUInt = "uint"
    toGLSL SFloat = "float"
    toGLSL SBool = "bool"

vecPrefix :: Scalar a -> String
vecPrefix SInt = "i"
vecPrefix SUInt = "u"
vecPrefix SFloat = ""
vecPrefix SBool = "b"

data Vect (n :: Nat) a where
    Vect :: NatR n -> Scalar a -> Vect n a

instance HasGLSL (Vect n a) where
    toGLSL (Vect (TS TZ) scal) =
        toGLSL scal
    toGLSL (Vect nat scal) =
        vecPrefix scal ++ "vec" ++
        show (natRToInt nat)

-- | Type of a square matrix.
data Mat (n :: Nat) where
    Mat :: NatR n -> Mat n

instance HasGLSL (Mat n) where
    toGLSL (Mat (TS TZ)) = toGLSL SFloat
    toGLSL (Mat nat) = "mat" ++ times 2 (show $ natRToInt nat)
      where
        times :: Int -> [a] -> [a]
        times i xs | i < 1     = []
                   | i == 1    = xs
                   | otherwise = xs ++ times (i-1) xs

-- | 'Data.Vec' Vec*s indexed over 'GHC.TypeLits.Nat'.
type family VecN (n :: Nat) (a :: *) :: * where
    VecN 1 a = a
    VecN 2 a = Vec2 a
    VecN 3 a = Vec3 a
    VecN 4 a = Vec4 a

-- | 'Data.Vec' Mat*s indexed over 'GHC.TypeLits.Nat'.
type family MatN (n :: Nat) :: * where
    MatN 1 = Float
    MatN 2 = Mat22 Float
    MatN 3 = Mat33 Float
    MatN 4 = Mat44 Float


---------------------------
-- HasType / KnownScalar --
---------------------------

-- | Anything that has a GLSL Type.
class HasGLSL a => HasType a where
    typeOf :: a -> Type a

-- | Types whose GLSL counterparts are scalars.
class HasType a => KnownScalar a where
    scalarType :: a -> Scalar a
    glScalarType :: a -> GL.DataType

instance HasType () where
    typeOf _ = UnitT
-- Scalars
instance HasType Int where
    typeOf _ = VectT (Vect (TS TZ) SInt)
instance HasType Word where
    typeOf _ = VectT (Vect (TS TZ) SUInt)
instance HasType Float where
    typeOf _ = VectT (Vect (TS TZ) SFloat)
instance HasType Bool where
    typeOf _ = VectT (Vect (TS TZ) SBool)
-- Function type
instance (HasType a, HasType b) => HasType (a -> b) where
    typeOf _ = typeOf (undefined :: a) :->: typeOf (undefined :: b)
-- Tuples
instance (HasType a, HasType b, HasGLSL a, HasGLSL b) => HasType (a, b) where
    typeOf _ = typeOf (undefined :: a) :*: typeOf (undefined :: b)
-- Vecs
instance KnownScalar a => HasType (Vec2 a) where
    typeOf _ =
        let len = TS (TS TZ)
            scalarT = scalarType (undefined :: a)
        in VectT (Vect len scalarT)
instance KnownScalar a => HasType (Vec3 a) where
    typeOf _ =
        let len = TS (TS (TS TZ))
            scalarT = scalarType (undefined :: a)
        in VectT (Vect len scalarT)
instance KnownScalar a => HasType (Vec4 a) where
    typeOf _ =
        let len = TS (TS (TS (TS TZ)))
            scalarT = scalarType (undefined :: a)
        in VectT (Vect len scalarT)
{- TODO: Causes overlapping instances
-- Matrices
instance HasType (Mat22 Float) where
    typeOf _ =
        let len = TS (TS TZ)
        in MatT (Mat len)
instance HasType (Mat33 Float) where
    typeOf _ =
        let len = TS (TS (TS TZ))
        in MatT (Mat len)
instance HasType (Mat44 Float) where
    typeOf _ =
        let len = TS (TS (TS (TS TZ)))
        in MatT (Mat len)
-}

instance KnownScalar Int where
    scalarType _ = SInt
    glScalarType _ = GL.Int
instance KnownScalar Word where
    scalarType _ = SUInt
    glScalarType _ = GL.UnsignedInt
instance KnownScalar Float where
    scalarType _ = SFloat
    glScalarType _ = GL.Float
instance KnownScalar Bool where
    scalarType _ = SBool
    glScalarType _ = GL.UnsignedByte


{-
-- | Any data type that can be converted
--   to a type known by the GPU.
class HasType gpu => ToGPU cpu gpu | cpu -> gpu where
    toGPU :: cpu -> gpu
-}

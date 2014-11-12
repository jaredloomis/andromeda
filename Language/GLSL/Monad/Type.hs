{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.GLSL.Monad.Type where

import Data.Proxy
import Data.Vec (Vec2, Vec3, Vec4, Mat44)
import Data.String (fromString)

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL.Raw (GLfloat)

import qualified Data.ByteString as B

type Name = B.ByteString

data Value (q :: Qualifier) (t :: Type) =
    Value Name (Proxy q) (Proxy t)

data Expression (t :: Type) =
    Expression B.ByteString (Proxy t)

data Array (q :: Qualifier) (t :: Type) =
    Array Name (Proxy q) (Proxy t)

-- | Representation of ShaderM qualifiers in
--   Haskell value level.
data Qualifier =
    Out
  | In
  | Uniform
  | None
  deriving Eq

-- = Representation of ShaderM qualifiers in
--   Haskell type level, using TypeLits.
outp :: Proxy Out
outp = Proxy
inp :: Proxy In
inp = Proxy
uniformp :: Proxy Uniform
uniformp = Proxy
nonep :: Proxy None
nonep = Proxy

-- | Representation of ShaderM s types in
--   Haskell value level.
data Type =
    GInt
  | GUInt
  | GFloat
  | GMat4
  | GVec4
  | GVec3
  | GVec2
  | GSampler2D
  | GSampler3D
  | GPair Type Type
  | GNoType
  deriving Eq

pairSuffix :: (Name, Name)
pairSuffix = ("pairA", "pairB")

instance Show Type where
    show GInt = "int"
    show GUInt = "uint"
    show GFloat = "float"
    show GMat4 = "mat4"
    show GVec4 = "vec4"
    show GVec3 = "vec3"
    show GVec2 = "vec2"
    show GSampler2D = "sampler2D"
    show GSampler3D = "sampler3D"
    show GPair{} = error "Type.hs: cannot show Pair"
    show GNoType = ""

-- = Representation of ShaderM types in
--   Haskell type level, using DataKinds.
int :: Proxy GInt
int = Proxy
uint :: Proxy GUInt
uint = Proxy
float :: Proxy GFloat
float = Proxy
mat4 :: Proxy GMat4
mat4 = Proxy
vec3 :: Proxy GVec3
vec3 = Proxy
vec2 :: Proxy GVec2
vec2 = Proxy
vec4 :: Proxy GVec4
vec4 = Proxy
sampler2D :: Proxy GSampler2D
sampler2D = Proxy
sampler3D :: Proxy GSampler3D
sampler3D = Proxy
pair :: Proxy a -> Proxy b -> Proxy (GPair a b)
pair _ _ = Proxy
typeOfP :: Proxy a -> Proxy (TypeOf a)
typeOfP _ = Proxy
typeOf :: a -> Proxy (TypeOf a)
typeOf _ = Proxy
notype :: Proxy GNoType
notype = Proxy

--------------------------
-- Types for CPU -> GPU --
--------------------------

data Sampler2D = Sampler2DInfo GL.TextureObject GL.TextureUnit
data Sampler3D = Sampler3DInfo GL.TextureObject GL.TextureUnit

data In t =
    InFloat (t -> [GLfloat]) Name
  | InInt (t -> [Int]) Name
  | InVec2 (t -> [Vec2 GLfloat]) Name
  | InVec3 (t -> [Vec3 GLfloat]) Name
  | InVec4 (t -> [Vec4 GLfloat]) Name
  | InMat4 (t -> [Mat44 GLfloat]) Name
  | InPair (In t) (In t)
  | InNone Name

data Uniform t =
    UniformFloat (t -> GLfloat) Name
  | UniformInt (t -> Int) Name
  | UniformVec2 (t -> Vec2 GLfloat) Name
  | UniformVec3 (t -> Vec3 GLfloat) Name
  | UniformVec4 (t -> Vec4 GLfloat) Name
  | UniformMat4 (t -> Mat44 GLfloat) Name
  | UniformSampler2D (t -> Sampler2D) Name
  | UniformSampler3D (t -> Sampler3D) Name
  | UniformPair (Uniform t) (Uniform t)

data Out =
    OutFloat Name
  | OutInt Name
  | OutBool Name
  | OutVec2 Name
  | OutVec3 Name
  | OutVec4 Name
  | OutMat4 Name
  | OutSampler2D Name
  | OutSampler3D Name
  | OutPair Out Out
  | OutNone Name
  deriving (Show, Eq)

-- | This is to allow for simple calling of
--   user-defined functions.
data Arg = forall a. (HasGPUCode a, ReadableQ a) => Arg a

pack :: (HasGPUCode a, ReadableQ a) => a -> Arg
pack = Arg

--------------------------------------------------
-- How to transfer CPU types to GPU (GLSL) code --
--------------------------------------------------

class HasGPUCode a where
    getGPUCode :: a -> B.ByteString
instance HasGPUCode (Value q t) where
    getGPUCode (Value name _ _) = name
instance HasGPUCode (Expression t) where
    getGPUCode (Expression str _) = str
instance HasGPUCode Arg where
    getGPUCode (Arg a) = getGPUCode a
instance HasGPUCode B.ByteString where
    getGPUCode = id
instance HasGPUCode Float where
    getGPUCode = fromString . show
instance HasGPUCode Double where
    getGPUCode = fromString . show
instance HasGPUCode Int where
    getGPUCode = fromString . show
instance HasGPUCode Integer where
    getGPUCode = fromString . show
instance HasGPUCode Bool where
    getGPUCode True = "true"
    getGPUCode False = "false"

----------------------------
-- Classes for Qualifiers --
----------------------------

class WritableQS (a :: Qualifier)
instance WritableQS 'Out
instance WritableQS 'None

class ReadableQS (a :: Qualifier)
instance ReadableQS 'In
instance ReadableQS 'Uniform
instance ReadableQS 'None

class WritableQ q
instance WritableQS q => WritableQ (Value q t)
instance WritableQS q => WritableQ (Array q t)
instance WritableQ (Expression t)

class ReadableQ q
instance ReadableQS q => ReadableQ (Value q t)
instance ReadableQS q => ReadableQ (Array q t)
instance ReadableQ (Expression t)
instance ReadableQ Arg
instance ReadableQ B.ByteString
instance ReadableQ Float
instance ReadableQ Double
instance ReadableQ Int
instance ReadableQ Integer
instance ReadableQ Bool

class LayoutQS (a :: Qualifier)
instance LayoutQS 'In
instance LayoutQS 'Uniform
instance LayoutQS 'Out

class LayoutQ q
instance LayoutQS q => LayoutQ (Value q t)

----------------------
-- Classes for Type --
----------------------

class NumT (t :: Type)

instance NumT GInt
instance NumT GUInt
instance NumT GFloat
instance NumT GVec4
instance NumT GVec3
instance NumT GVec2
instance NumT GMat4

class ScalarT (t :: Type)

instance ScalarT GInt
instance ScalarT GUInt
instance ScalarT GFloat

class VecT (t :: Type)

instance VecT GVec2
instance VecT GVec3
instance VecT GVec4

--------------
-- Families --
--------------

-- = TypeOf type family.

type family TypeOf a :: Type
type instance TypeOf (Value q t) = t
type instance TypeOf (Expression t) = t
type instance TypeOf (Array q t) = t
type instance TypeOf Float = GFloat
type instance TypeOf Double = GFloat
type instance TypeOf Int = GInt
type instance TypeOf Integer = GInt

-- = QualifierOf type family.

type family QualifierOf a :: Qualifier
type instance QualifierOf (Value q t) = q
type instance QualifierOf (Expression t) = 'None
type instance QualifierOf (Array q t) = q

-- = Math type family.

type family Math (a :: Type) (b :: Type) :: Type

type instance Math a a = a

-- primitives
type instance Math GFloat GInt = GFloat
type instance Math GInt GFloat = GFloat
type instance Math GFloat GInt = GFloat
type instance Math GUInt GFloat = GFloat
type instance Math GInt GUInt = GInt
type instance Math GUInt GInt = GInt

-- scalar + mat4 = mat4 (???)
type instance Math GInt GMat4 = GMat4
type instance Math GFloat GMat4 = GMat4
type instance Math GUInt GMat4 = GMat4
type instance Math GMat4 GInt = GMat4
type instance Math GMat4 GFloat = GMat4
type instance Math GMat4 GUInt = GMat4

-- vec + scalar = vec (???)
type instance Math GVec4 GFloat = GVec4
type instance Math GVec4 GUInt = GVec4
type instance Math GVec4 GInt = GVec4
type instance Math GFloat GVec4 = GVec4
type instance Math GUInt GVec4 = GVec4
type instance Math GInt GVec4 = GVec4
type instance Math GVec3 GFloat = GVec3
type instance Math GVec3 GUInt = GVec3
type instance Math GVec3 GInt = GVec3
type instance Math GFloat GVec3 = GVec3
type instance Math GUInt GVec3 = GVec3
type instance Math GInt GVec3 = GVec3
type instance Math GVec2 GFloat = GVec2
type instance Math GVec2 GUInt = GVec2
type instance Math GVec2 GInt = GVec2
type instance Math GFloat GVec2 = GVec2
type instance Math GUInt GVec2 = GVec2
type instance Math GInt GVec2 = GVec2

-- mat4 + vec = vec (???)
type instance Math GMat4 GVec4 = GVec4
type instance Math GMat4 GVec3 = GVec3
type instance Math GMat4 GVec2 = GVec2
type instance Math GVec4 GMat4 = GVec4
type instance Math GVec3 GMat4 = GVec3
type instance Math GVec2 GMat4 = GVec2

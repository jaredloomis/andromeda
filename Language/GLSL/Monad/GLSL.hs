{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
module Language.GLSL.Monad.GLSL where

import Data.Proxy (Proxy(..))
import GHC.TypeLits
import Data.Vec (Vec2, Vec3, Vec4, Mat44)

import Control.Applicative (Applicative)
import Control.Monad.Writer

import Data.String (fromString)
import qualified Data.ByteString as B

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL.Raw (GLfloat)

-- TODO: Use ByteString builder,
--       there are many inefficient
--       appends that could be /O(1)/.

---------------
-- Execution --
---------------

generateGLSL :: ShaderM s t a -> B.ByteString
generateGLSL = programBString . execShaderM

execShaderM :: ShaderM s t a -> [GLSLUnit]
execShaderM = fst . runShaderM

evalShaderM :: ShaderM s t a -> GLSLInfo t
evalShaderM = snd . runShaderM

runShaderM :: ShaderM s t a -> ([GLSLUnit], GLSLInfo t)
runShaderM = execWriter . sInnerShaderM

------------------
-- Shader types --
------------------

type VertexShaderM t a = ShaderM GL.VertexShader t a
type GeometryShaderM t a = ShaderM GL.GeometryShader t a
type FragmentShaderM t a = ShaderM GL.FragmentShader t a

newtype ShaderM s t a = ShaderM {
    sInnerShaderM :: Writer ([GLSLUnit], GLSLInfo t) a
    }
    deriving (Functor, Applicative, Monad)
instance MonadWriter ([GLSLUnit], GLSLInfo t) (ShaderM s t) where
    listen (ShaderM glsl) = ShaderM $ listen glsl
    pass (ShaderM glsl) = ShaderM $ pass glsl
    tell = ShaderM . tell
    writer = ShaderM . writer

class ShaderType (s :: GL.ShaderType) where
    type InArgs s gt (t :: Type) :: *
    type UniformArgs s gt (t :: Type) :: *

    layoutDecl :: (GPU t, Reify t Type, Reify q Qualifier) =>
        [B.ByteString] -> Proxy q -> Proxy t ->
        B.ByteString -> ShaderM s gt (Value q t)

    layoutIn :: (GPU t, Reify t Type) =>
        [B.ByteString] -> Proxy t -> InArgs s gt t ->
        ShaderM s gt (Value 'In t)
    layoutUniform :: (GPU t, Reify t Type) =>
        [B.ByteString] -> Proxy t -> UniformArgs s gt t ->
        ShaderM s gt (Value 'Uniform t)
    layoutOut :: (GPU t, Reify t Type) =>
        [B.ByteString] -> Proxy t -> B.ByteString ->
        ShaderM s gt (Value 'Out t)

    none :: (GPU t, Reify t Type) =>
        Proxy t -> B.ByteString ->
        ShaderM s gt (Value 'None t)
    none = layoutDecl [] Proxy

    inn :: (GPU t, Reify t Type) =>
        Proxy t -> InArgs s gt t -> ShaderM s gt (Value 'In t)
    inn = layoutIn []

    uniform :: (GPU t, Reify t Type) =>
        Proxy t -> UniformArgs s gt t -> ShaderM s gt (Value 'Uniform t)
    uniform = layoutUniform []

    out :: (GPU t, Reify t Type) =>
        Proxy t -> B.ByteString -> ShaderM s gt (Value 'Out t)
    out = layoutOut []

instance ShaderType GL.VertexShader where
    type InArgs GL.VertexShader gt t = (B.ByteString, gt -> [CPU t])
    type UniformArgs GL.VertexShader gt t = (B.ByteString, gt -> CPU t)

    layoutDecl layouts qualifier glType name = do
        ltell $ Decl (Layout layouts) (reify qualifier) (reify glType) name
        return $ Value name qualifier glType

    layoutIn layouts glType (name, values) = do
        logIn glType values name
        layoutDecl layouts Proxy glType name

    layoutUniform layouts glType (name, value) = do
        logUniform glType value name
        layoutDecl layouts Proxy glType name

    layoutOut layouts glType name = do
        logOut glType name
        layoutDecl layouts Proxy glType name

instance ShaderType GL.TessControlShader where
    type InArgs GL.TessControlShader gt t = B.ByteString
    type UniformArgs GL.TessControlShader gt t = (B.ByteString, gt -> CPU t)

    layoutDecl layouts qualifier glType name = do
        ltell $ Decl (Layout layouts) (reify qualifier) (reify glType) name
        ShaderM . return $ Value name qualifier glType

    layoutIn layouts glType name = do
        logIn glType (const []) name
        layoutDecl layouts Proxy glType name

    layoutUniform layouts glType (name, value) = do
        logUniform glType value name
        layoutDecl layouts Proxy glType name

    layoutOut layouts glType name = do
        logOut glType name
        layoutDecl layouts Proxy glType name

instance ShaderType GL.TessEvaluationShader where
    type InArgs GL.TessEvaluationShader gt t = B.ByteString
    type UniformArgs GL.TessEvaluationShader gt t = (B.ByteString, gt -> CPU t)

    layoutDecl layouts qualifier glType name = do
        ltell $ Decl (Layout layouts) (reify qualifier) (reify glType) name
        ShaderM . return $ Value name qualifier glType

    layoutIn layouts glType name = do
        logIn glType (const []) name
        layoutDecl layouts Proxy glType name

    layoutUniform layouts glType (name, value) = do
        logUniform glType value name
        layoutDecl layouts Proxy glType name

    layoutOut layouts glType name = do
        logOut glType name
        layoutDecl layouts Proxy glType name

instance ShaderType GL.GeometryShader where
    type InArgs GL.GeometryShader gt t = B.ByteString
    type UniformArgs GL.GeometryShader gt t = (B.ByteString, gt -> CPU t)

    layoutDecl layouts qualifier glType name = do
        ltell $ Decl (Layout layouts) (reify qualifier) (reify glType) name
        ShaderM . return $ Value name qualifier glType

    layoutIn layouts glType name = do
        logIn glType (const []) name
        layoutDecl layouts Proxy glType name

    layoutUniform layouts glType (name, value) = do
        logUniform glType value name
        layoutDecl layouts Proxy glType name

    layoutOut layouts glType name = do
        logOut glType name
        layoutDecl layouts Proxy glType name

instance ShaderType GL.FragmentShader where
    type InArgs GL.FragmentShader gt t = B.ByteString
    type UniformArgs GL.FragmentShader gt t = (B.ByteString, gt -> CPU t)

    layoutDecl layouts qualifier glType name = do
        ltell $ Decl (Layout layouts) (reify qualifier) (reify glType) name
        return $ Value name qualifier glType

    layoutIn layouts glType name = do
        logIn glType (const []) name
        layoutDecl layouts Proxy glType name

    layoutUniform layouts glType (name, value) = do
        logUniform glType value name
        layoutDecl layouts Proxy glType name

    layoutOut layouts glType name = do
        logOut glType name
        layoutDecl layouts Proxy glType name

-------------------
-- Logging Stuff --
-------------------

data GLSLInfo t = GLSLInfo [In t] [Uniform t] [Out]

instance Monoid (GLSLInfo t) where
    mempty = GLSLInfo [] [] []
    mappend (GLSLInfo in1 uni1 out1) (GLSLInfo in2 uni2 out2) =
        GLSLInfo (in1 ++ in2) (uni1 ++ uni2) (out1 ++ out2)

inInfo :: In t -> GLSLInfo t
inInfo i = GLSLInfo [i] [] []

uniformInfo :: Uniform t -> GLSLInfo t
uniformInfo u = GLSLInfo [] [u] []

outInfo :: Out -> GLSLInfo t
outInfo o = GLSLInfo [] [] [o]

data In t =
    InFloat (t -> [GLfloat]) Name
  | InInt (t -> [Int]) Name
  | InVec2 (t -> [Vec2 GLfloat]) Name
  | InVec3 (t -> [Vec3 GLfloat]) Name
  | InVec4 (t -> [Vec4 GLfloat]) Name
  | InNone Name
  -- Unlikely, but...
  | InMat4 (t -> [Mat44 GLfloat]) Name

data Sampler2D = Sampler2DInfo GL.TextureObject GL.TextureUnit
data Sampler3D = Sampler3DInfo GL.TextureObject GL.TextureUnit

data Uniform t =
    UniformFloat (t -> GLfloat) Name
  | UniformInt (t -> Int) Name
  | UniformVec2 (t -> Vec2 GLfloat) Name
  | UniformVec3 (t -> Vec3 GLfloat) Name
  | UniformVec4 (t -> Vec4 GLfloat) Name
  | UniformMat4 (t -> Mat44 GLfloat) Name
  | UniformSampler2D (t -> Sampler2D) Name
  | UniformSampler3D (t -> Sampler3D) Name

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
  | OutNone Name
  deriving (Show, Eq)

class GPU (a :: Type) where
    type CPU a :: *
    inConstr :: Proxy a -> ((t -> [CPU a]) -> Name -> GLSLInfo t)
    uniformConstr :: Proxy a -> ((t -> CPU a) -> Name -> GLSLInfo t)
    outConstr :: Proxy a -> (Name -> GLSLInfo t)

    logIn :: Proxy a -> (t -> [CPU a]) -> Name -> ShaderM s t ()
    logIn proxy values = rtell . inConstr proxy values
    logUniform :: Proxy a -> (t -> CPU a) -> Name -> ShaderM s t ()
    logUniform proxy value = rtell . uniformConstr proxy value
    logOut :: Proxy a -> Name -> ShaderM s t ()
    logOut proxy = rtell . outConstr proxy

instance GPU 'Int where
    type CPU 'Int = Int
    inConstr _ values = inInfo . InInt values
    uniformConstr _ value = uniformInfo . UniformInt value
    outConstr _ = outInfo . OutInt

instance GPU 'Float where
    type CPU 'Float = GLfloat
    inConstr _ values = inInfo . InFloat values
    uniformConstr _ value = uniformInfo . UniformFloat value
    outConstr _ = outInfo . OutFloat

instance GPU 'Vec2 where
    type CPU 'Vec2 = Vec2 GLfloat
    inConstr _ values = inInfo . InVec2 values
    uniformConstr _ value = uniformInfo . UniformVec2 value
    outConstr _ = outInfo . OutVec2
instance GPU 'Vec3 where
    type CPU 'Vec3 = Vec3 GLfloat
    inConstr _ values = inInfo . InVec3 values
    uniformConstr _ value = uniformInfo . UniformVec3 value
    outConstr _ = outInfo . OutVec3
instance GPU 'Vec4 where
    type CPU 'Vec4 = Vec4 GLfloat
    inConstr _ values = inInfo . InVec4 values
    uniformConstr _ value = uniformInfo . UniformVec4 value
    outConstr _ = outInfo . OutVec4

instance GPU Mat4 where
    type CPU Mat4 = Mat44 GLfloat
    inConstr _ values = inInfo . InMat4 values
    uniformConstr _ value = uniformInfo . UniformMat4 value
    outConstr _ = outInfo . OutMat4

instance GPU 'Sampler2D where
    type CPU 'Sampler2D = Sampler2D
    inConstr _ _ _ = error "ShaderM.inConstr: sampler2D cannot be an in var."
    uniformConstr _ value = uniformInfo . UniformSampler2D value
    outConstr _ = outInfo . OutSampler2D

instance GPU 'Sampler3D where
    type CPU 'Sampler3D = Sampler3D
    inConstr _ _ _ = error "ShaderM.inConstr: sampler3D cannot be an in var."
    uniformConstr _ value = uniformInfo . UniformSampler3D value
    outConstr _ = outInfo . OutSampler3D

instance GPU NoType where
    type CPU NoType = ()
    inConstr _ _ = inInfo . InNone 
    uniformConstr _ _ _ =
        error "ShaderM.uniformConstr: notype cannot be an uniform."
    outConstr _ = outInfo . OutNone

-----------
-- Types --
-----------

data GLSLUnit =
    Version B.ByteString
  | Decl Layout Qualifier Type Name
  | AssignStatement Name B.ByteString
  | Action B.ByteString
  deriving Eq

type Name = B.ByteString

data SymbolProxy (a :: Symbol) = SProxy
data NatProxy (a :: Nat) = NProxy

data Value (q :: Qualifier) (t :: Type) =
    Value Name (Proxy q) (Proxy t)

data Array (q :: Qualifier) (t :: Type) =
    Array Name (Proxy q) (Proxy t) Int

data Expression (t :: Type) =
    Expression B.ByteString (Proxy t)

newtype Layout = Layout [B.ByteString]
  deriving (Show, Eq, Monoid)

data Arg = forall a. (HasBString a, ReadableQ a) => Arg a

pack :: (HasBString a, ReadableQ a) => a -> Arg
pack = Arg

-- | Representation of ShaderM qualifiers in
--   Haskell value level.
data Qualifier =
    Out
  | In
  | Uniform
  | None
  deriving Eq

type QualifierP = SymbolProxy

-- = Representation of ShaderM qualifiers in
--   Haskell type level, using TypeLits.
outp :: Proxy 'Out
outp = Proxy
inp :: Proxy 'In
inp = Proxy
uniformp :: Proxy 'Uniform
uniformp = Proxy
nonep :: Proxy 'None
nonep = Proxy

-- | Representation of ShaderM s types in
--   Haskell value level.
data Type =
    Int
  | UInt
  | Float
  | Mat4
  | Vec4
  | Vec3
  | Vec2
  | Sampler2D
  | Sampler3D
  | NoType
  deriving Eq

instance Show Type where
    show Int = "int"
    show UInt = "uint"
    show Float = "float"
    show Mat4 = "mat4"
    show Vec4 = "vec4"
    show Vec3 = "vec3"
    show Vec2 = "vec2"
    show Sampler2D = "sampler2D"
    show Sampler3D = "sampler3D"
    show NoType = ""

type TypeP = SymbolProxy

-- = Representation of ShaderM s types in
--   Haskell type level, using DataKinds.
int :: Proxy 'Int
int = Proxy
uint :: Proxy 'UInt
uint = Proxy
float :: Proxy 'Float
float = Proxy
mat4 :: Proxy Mat4
mat4 = Proxy
vec3 :: Proxy 'Vec3
vec3 = Proxy
vec2 :: Proxy 'Vec2
vec2 = Proxy
vec4 :: Proxy 'Vec4
vec4 = Proxy
sampler2D :: Proxy 'Sampler2D
sampler2D = Proxy
sampler3D :: Proxy 'Sampler3D
sampler3D = Proxy
notype :: Proxy 'NoType
notype = Proxy

-- = TypeLits to B.ByteString stuff.

class Reify a b | a -> b where
    reify :: Proxy a -> b

qualifierSymbol :: KnownSymbol q => SymbolProxy q -> Qualifier
qualifierSymbol q =
    case symbolVal q of
        "uniform" -> Uniform
        "in" -> In
        "out" -> Out
        "none" -> None
        _ -> error "Primitive.toTypeQ"

instance Reify 'Int Type where
    reify _ = Int
instance Reify 'UInt Type where
    reify _ = UInt
instance Reify 'Float Type where
    reify _ = Float
instance Reify 'Mat4 Type where
    reify _ = Mat4
instance Reify 'Vec4 Type where
    reify _ = Vec4
instance Reify 'Vec3 Type where
    reify _ = Vec3
instance Reify 'Vec2 Type where
    reify _ = Vec2
instance Reify 'Sampler2D Type where
    reify _ = Sampler2D
instance Reify 'Sampler3D Type where
    reify _ = Sampler3D
instance Reify NoType Type where
    reify _ = NoType

instance Reify 'In Qualifier where
    reify _ = In
instance Reify 'Out Qualifier where
    reify _ = Out
instance Reify 'Uniform Qualifier where
    reify _ = Uniform
instance Reify 'None Qualifier where
    reify _ = None

{-
typeSymbol :: Proxy Type -> Type
typeSymbol t =
    case symbolVal t of
        "int" -> Int
        "uint" -> UInt
        "float" -> Float
        "mat4" -> Mat4
        "vec4" -> Vec4
        "vec3" -> Vec3
        "vec2" -> Vec2
        "sampler2D" -> Sampler2D
        "sampler3D" -> Sampler3D
        "notype" -> NoType
        _ -> error "Primitive.toTypeT"
-}

---------------------
-- BShow instances --
---------------------

class BShow a where
    bshow :: a -> B.ByteString

instance BShow Type where
    bshow Int = "int"
    bshow UInt = "uint"
    bshow Float = "float"
    bshow Mat4 = "mat4"
    bshow Vec2 = "vec2"
    bshow Vec3 = "vec3"
    bshow Vec4 = "vec4"
    bshow Sampler2D = "sampler2D"
    bshow Sampler3D = "sampler3D"
    bshow NoType = ""

instance BShow Qualifier where
    bshow In = "in"
    bshow Out = "out"
    bshow Uniform = "uniform"
    bshow None = B.empty

instance BShow Layout where
    bshow (Layout layouts)
        | not . null $ layouts =
            "layout" <> paren (B.intercalate ", " layouts)
            <> " "
        | otherwise = B.empty

instance BShow GLSLUnit where
    bshow (Version v) = "#version " <> v <> "\n"
    bshow (Decl layout None glType name) =
        bshow layout <>
        bshow glType <> " " <> name <>
        " = " <> defaultValue glType <>
        ";\n"
    bshow (Decl layout qualifier glType name) =
        bshow layout <>
        bshow qualifier <> " " <>
        bshow glType <> " " <>
        name <> ";\n"
    bshow (AssignStatement a b) =
        a <> " = " <> b <> ";\n"
    bshow (Action a) = a <> ";\n"

defaultValue :: Type -> B.ByteString
defaultValue Int = "0"
defaultValue Float = "0.0"
defaultValue Mat4 = "mat4(1.0)"
defaultValue Vec3 = "vec3(0.0, 0.0, 0.0)"
defaultValue Vec2 = "vec2(0.0, 0.0)"
defaultValue UInt = "0"
defaultValue Vec4 = "vec4(0.0, 0.0, 0.0, 0.0)"
defaultValue Sampler2D =
    error $ "ShaderM.defaultValue: Sampler2D does" ++
            "not have a default value."
defaultValue Sampler3D =
    error $ "ShaderM.defaultValue: Sampler3D does" ++
            "not have a default value."
defaultValue NoType =
    error $ "ShaderM.defaultValue: NoType does" ++
            "not have a default value."
-------------
-- Classes --
-------------

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

class HasBString a where
    getBString :: a -> B.ByteString
instance HasBString (Value q t) where
    getBString (Value name _ _) = name
instance HasBString (Expression t) where
    getBString (Expression str _) = str
instance HasBString Arg where
    getBString (Arg a) = getBString a
instance HasBString B.ByteString where
    getBString = id
instance HasBString Float where
    getBString = fromString . show
instance HasBString Double where
    getBString = fromString . show
instance HasBString Int where
    getBString = fromString . show
instance HasBString Integer where
    getBString = fromString . show
instance HasBString Bool where
    getBString True = "true"
    getBString False = "false"

--------------------
-- Vec conversion --
--------------------

type family VecLength (v :: Type) :: Nat where
    VecLength 'Float = 1
    VecLength 'Vec2  = 2
    VecLength 'Vec3  = 3
    VecLength 'Vec4  = 4

type family VecLengthU (n :: Nat) :: Type where
    VecLengthU 1 = 'Float
    VecLengthU 2 = 'Vec2
    VecLengthU 3 = 'Vec3
    VecLengthU 4 = 'Vec4

-- = Swizzling

data Index (len :: Nat) (maxI :: Nat) where
    X :: Index 1 1
    Y :: Index 1 2
    Z :: Index 1 3
    W :: Index 1 4
    Linked :: Index len1 max1 -> Index len2 max2 ->
              Index (len1 + len2) (If (max1 <=? max2) max2 max1)

(.&) :: Index len1 max1 -> Index len2 max2 ->
        Index (len1 + len2) (If (max1 <=? max2) max2 max1)
(.&) = Linked

type family If (condition :: Bool) (yes :: Nat) (no :: Nat) :: Nat where
    If True yes no = yes
    If False yes no = no

instance BShow (Index len s) where
    bshow X = "x"
    bshow Y = "y"
    bshow Z = "z"
    bshow W = "w"
    bshow (Linked list1 list2) = bshow list1 <> bshow list2

(.@) :: forall a maxI len.
        (HasBString a, ReadableQ a,
         maxI <= VecLength (TypeOf a)) =>
    a -> Index len maxI -> Expression (VecLengthU len)
(.@) vec index =
    let swizzleStr = bshow index
    in Expression (paren $ getBString vec <> "." <> swizzleStr)
        Proxy
infixl 8 .@

-- = Vec "concatenation"

vConcat :: forall a at b bt ct.
           (HasBString a, ReadableQ a, TypeOf a ~ at,
            HasBString b, ReadableQ b, TypeOf b ~ bt,
            VecLengthU (VecLength at + VecLength bt) ~ ct,
            Reify ct Type) =>
     a -> b -> Expression ct
vConcat left right =
    let resultProxy = Proxy :: Proxy ct
        resultCtor = show (reify resultProxy :: Type)
    in Expression (paren $ fromString resultCtor <>
        (paren $ getBString left <> ", " <> getBString right))
        Proxy

(+.+) :: forall a at b bt ct.
           (HasBString a, ReadableQ a, TypeOf a ~ at,
            HasBString b, ReadableQ b, TypeOf b ~ bt,
            VecLengthU (VecLength at + VecLength bt) ~ ct,
            Reify ct Type) =>
     a -> b -> Expression ct
(+.+) = vConcat
infixr 5 +.+

--------------------
-- Array indexing --
--------------------

class IndexT t
instance IndexT Int
instance IndexT Integer
instance IndexT (Expression 'Int)
instance IndexT (Value q 'Int)
instance IndexT (Expression UInt)
instance IndexT (Value q UInt)

(.!) :: (ReadableQ a, HasBString a, IndexT a) =>
    Array q t -> a -> Expression t
(.!) (Array name _ _ _) i =
    Expression (name <> "[" <> getBString i <> "]") Proxy

---------------------------
-- Classes and instances --
---------------------------

class NumT (t :: Type)

instance NumT 'Int
instance NumT UInt
instance NumT 'Float
instance NumT 'Vec4
instance NumT 'Vec3
instance NumT 'Vec2
instance NumT 'Mat4

class ScalarT (t :: Type)

instance ScalarT 'Int
instance ScalarT UInt
instance ScalarT 'Float

class VecT (t :: Type)

instance VecT 'Vec2
instance VecT 'Vec3
instance VecT 'Vec4

--------------
-- Families --
--------------

-- = TypeOf type family.

type family TypeOf a :: Type
type instance TypeOf (Value q t) = t
type instance TypeOf (Expression t) = t
type instance TypeOf (Array q t) = t
type instance TypeOf Float = 'Float
type instance TypeOf Double = 'Float
type instance TypeOf Int = 'Int
type instance TypeOf Integer = 'Int

-- = QualifierOf type family.

type family QualifierOf a :: Qualifier
type instance QualifierOf (Value q t) = q
type instance QualifierOf (Expression t) = 'None
type instance QualifierOf (Array q t) = q

-- = Math type family.

type family Math (a :: Type) (b :: Type) :: Type

type instance Math a a = a

-- primitives
type instance Math 'Float 'Int = 'Float
type instance Math 'Int 'Float = 'Float
type instance Math 'Float 'Int = 'Float
type instance Math UInt 'Float = 'Float
type instance Math 'Int UInt = 'Int
type instance Math UInt 'Int = 'Int

-- scalar + mat4 = mat4 (???)
type instance Math 'Int Mat4 = Mat4
type instance Math 'Float Mat4 = Mat4
type instance Math UInt Mat4 = Mat4
type instance Math Mat4 'Int = Mat4
type instance Math Mat4 'Float = Mat4
type instance Math Mat4 UInt = Mat4

-- vec + scalar = vec (???)
type instance Math 'Vec4 'Float = 'Vec4
type instance Math 'Vec4 UInt = 'Vec4
type instance Math 'Vec4 'Int = 'Vec4
type instance Math 'Float 'Vec4 = 'Vec4
type instance Math UInt 'Vec4 = 'Vec4
type instance Math 'Int 'Vec4 = 'Vec4
type instance Math 'Vec3 'Float = 'Vec3
type instance Math 'Vec3 UInt = 'Vec3
type instance Math 'Vec3 'Int = 'Vec3
type instance Math 'Float 'Vec3 = 'Vec3
type instance Math UInt 'Vec3 = 'Vec3
type instance Math 'Int 'Vec3 = 'Vec3
type instance Math 'Vec2 'Float = 'Vec2
type instance Math 'Vec2 UInt = 'Vec2
type instance Math 'Vec2 'Int = 'Vec2
type instance Math 'Float 'Vec2 = 'Vec2
type instance Math UInt 'Vec2 = 'Vec2
type instance Math 'Int 'Vec2 = 'Vec2

-- mat4 + vec = vec (???)
type instance Math Mat4 'Vec4 = 'Vec4
type instance Math Mat4 'Vec3 = 'Vec3
type instance Math Mat4 'Vec2 = 'Vec2
type instance Math 'Vec4 Mat4 = 'Vec4
type instance Math 'Vec3 Mat4 = 'Vec3
type instance Math 'Vec2 Mat4 = 'Vec2

---------------------
-- ShaderM Codegen --
---------------------

programBString :: [GLSLUnit] -> B.ByteString
programBString xs =
    let (top, bottom) = filterTop xs
        (versions, top') = filterVersion top
    in bshow (head versions) <>
       B.concat (map bshow top') <>
       "\nvoid main(){\n" <>
       B.concat (map bshow bottom) <>
       "}"

filterVersion :: [GLSLUnit] -> ([GLSLUnit], [GLSLUnit])
filterVersion (v@Version{} : xs) =
    let (versions, others) = filterVersion xs
    in (v : versions, others)
filterVersion (x : xs) =
    let (versions, others) = filterVersion xs
    in (versions, x : others)
filterVersion [] = ([], [])

-- | Filter declarations that appear at
--   the top of the file.
filterTop :: [GLSLUnit] -> ([GLSLUnit], [GLSLUnit])
filterTop (v@(Version{}) : xs) =
    let (top, bottom) = filterTop xs
    in (v : top, bottom)
filterTop (u@(Decl _ Uniform _ _) : xs) =
    let (top, bottom) = filterTop xs
    in (u : top, bottom)
filterTop (i@(Decl _ In _ _) : xs) =
    let (top, bottom) = filterTop xs
    in (i : top, bottom)
filterTop (o@(Decl _ Out _ _) : xs) =
    let (top, bottom) = filterTop xs
    in (o : top, bottom)
filterTop (x : xs) =
    let (top, bottom) = filterTop xs
    in (top, x : bottom)
filterTop [] = ([], [])

----------------
-- Assignment --
----------------

(#=) :: (HasBString a, HasBString b,
         WritableQ a, ReadableQ b,
         TypeOf a ~ TypeOf b) =>
    a -> b -> ShaderM s t ()
(#=) to from =
    ltell $ AssignStatement (getBString to) (getBString from)
infixr 1 #=

($=) :: (HasBString a, HasBString b,
         WritableQ a, ReadableQ b,
         TypeOf a ~ TypeOf b) =>
    ShaderM s t a -> b -> ShaderM s t a
($=) to from = do
    toVal <- to
    toVal #= from
    return toVal
infixr 1 $=

------------------------
-- Numeric operations --
------------------------

(.+) :: (HasBString a, HasBString b,
         ReadableQ a, ReadableQ b) =>
    a -> b -> Expression (Math (TypeOf a) (TypeOf b))
(.+) left right =
    Expression (paren (getBString left) <> " + " <> paren (getBString right))
                Proxy
infixl 6 .+

(.-) :: (HasBString a, HasBString b,
         ReadableQ a, ReadableQ b) =>
    a -> b -> Expression (Math (TypeOf a) (TypeOf b))
(.-) left right =
    Expression (paren (getBString left) <> " - " <> paren (getBString right))
                Proxy
infixl 6 .-

(.*) :: (HasBString a, HasBString b,
         ReadableQ a, ReadableQ b) =>
    a -> b -> Expression (Math (TypeOf a) (TypeOf b))
(.*) left right =
    Expression (paren (getBString left) <> " * " <> paren (getBString right))
                Proxy
infixl 7 .*

(./) :: (HasBString a, HasBString b,
         ReadableQ a, ReadableQ b) =>
    a -> b -> Expression (Math (TypeOf a) (TypeOf b))
(./) left right =
    Expression (paren (getBString left) <> " / " <> paren (getBString right))
                Proxy
infixl 7 ./

--------------------
-- ShaderM Built-ins --
--------------------

clamp :: (HasBString value, ReadableQ value, NumT (TypeOf value),
          HasBString bottom, ReadableQ bottom, NumT (TypeOf bottom),
          HasBString top, ReadableQ top, NumT (TypeOf top),
          TypeOf value ~ TypeOf bottom,
          TypeOf value ~ TypeOf top) =>
    value -> bottom -> top -> Expression (TypeOf value)
clamp value bottom top =
    Expression ("clamp(" <> getBString value <> "," <>
                            getBString bottom <> "," <>
                            getBString top <> ")") Proxy

transpose :: (HasBString mat, ReadableQ mat,
              TypeOf mat ~ Mat4) =>
    mat -> Expression Mat4
transpose matrix =
    Expression ("transpose(" <> getBString matrix <> ")") Proxy

inverse :: (HasBString mat, ReadableQ mat,
            TypeOf mat ~ Mat4) =>
    mat -> Expression Mat4
inverse matrix =
    Expression ("inverse(" <> getBString matrix <> ")") Proxy

vlength :: (HasBString vec, ReadableQ vec,
            VecT (TypeOf vec)) =>
    vec -> Expression 'Float
vlength vec =
    Expression ("length(" <> getBString vec <> ")") Proxy


normalize :: (HasBString vec, ReadableQ vec,
              VecT (TypeOf vec)) =>
    vec -> Expression (TypeOf vec)
normalize vec =
    Expression ("normalize(" <> getBString vec <> ")") Proxy

reflect :: (HasBString vec, ReadableQ vec,
            VecT (TypeOf vec)) =>
    vec -> vec -> Expression 'Float
reflect veca vecb =
    Expression ("reflect(" <> getBString veca <> ", " <> getBString vecb <> ")")
               Proxy

class IsSampler (s :: Type)
instance IsSampler 'Sampler2D
instance IsSampler 'Sampler3D

texture :: (HasBString tex, ReadableQ tex,
            IsSampler (TypeOf tex),
            HasBString vec, ReadableQ vec,
            VecT (TypeOf vec)) =>
    tex -> vec -> Expression 'Vec4
texture tex vec =
    Expression ("texture(" <> getBString tex <> ", " <> getBString vec <> ")")
       Proxy

glPosition :: Value 'None 'Vec4
glPosition = builtIn vec4 "gl_Position"

-- = For loops.

forSM :: Int -> Int -> (Value 'None 'Int -> ShaderM s t ()) -> ShaderM s t ()
forSM start end action = do
    ltell . Action $ "for(int i = " <> fromString (show start) <> "; " <>
                          "i <= " <> fromString (show end) <> "; i++)\n{"
    action $ Value "i" Proxy Proxy
    ltell . Action $ "}"

forSM_ :: Int -> Int -> ShaderM s t () -> ShaderM s t ()
forSM_ start end action = do
    ltell . Action $ "for(int i = " <> fromString (show start) <> "; " <>
                          "i <= " <> fromString (show end) <> "; i++)\n{"
    action
    ltell . Action $ "}"

-- = If statements.

-- TODO: Which types are truthy?
class Truthy (a :: Type)
instance Truthy 'Int
instance Truthy UInt
instance Truthy 'Float

ifS :: (HasBString bool, Truthy (TypeOf bool)) =>
    bool -> ShaderM s t () -> ShaderM s t ()
ifS condition action = do
    ltell . Action $ "if(" <> getBString condition <> ") {\n"
    action
    ltell . Action $ "}"

ifElse :: (HasBString bool, Truthy (TypeOf bool)) =>
    bool -> ShaderM s t () -> ShaderM s t () -> ShaderM s t ()
ifElse condition yes no = do
    ifS condition yes
    ltell . Action $ "else {\n"
    no
    ltell . Action $ "}"

--------------------------
-- Function application --
--------------------------

call :: B.ByteString -> ShaderM a b ()
call name = ltell . Action $ name <> "()"

(.$) :: (HasBString a,
         ReadableQ a) =>
    B.ByteString -> [a] -> Expression b
(.$) = apply
infixr 3 .$

(<.) :: (HasBString a,
         ReadableQ a) =>
    B.ByteString -> a -> Expression b
(<.) func x = apply func [x]
infixr 3 <.

apply :: (HasBString a,
          ReadableQ a) =>
    B.ByteString -> [a] -> Expression b
apply func args = Expression
    (func <> paren (B.intercalate ", " $ map getBString args))
    Proxy

----------------------
-- Declaring Arrays --
----------------------

-- TODO better.

declArray :: (Reify t Type, Reify q Qualifier) =>
    [B.ByteString] -> Proxy q -> Proxy t -> B.ByteString -> Int ->
    ShaderM s gt (Array q t)
declArray layouts qualifier glType name len = do
    let fullName = name <> "[" <> fromString (show len) <> "]"
    ltell $ Decl (Layout layouts) (reify qualifier) (reify glType) fullName
    return $ Array name qualifier glType len

layoutUniformArray :: (GPU t, Reify t Type) =>
    [B.ByteString] ->
    Proxy t -> B.ByteString -> Int -> (gt -> [CPU t]) ->
    ShaderM s gt (Array 'Uniform t)
layoutUniformArray layouts glType name len values = do
    logUniformArray len glType values name
    declArray layouts (Proxy :: Proxy 'Uniform) glType name len

uniformArray :: (GPU t, Reify t Type) =>
    Proxy t -> B.ByteString -> Int -> (gt -> [CPU t]) -> ShaderM s gt (Array 'Uniform t)
uniformArray glType name len values = do
    logUniformArray len glType values name
    declArray [] (Proxy :: Proxy 'Uniform) glType name len

logUniformArray :: GPU t =>
    Int ->
    Proxy t -> (gt -> [CPU t]) -> B.ByteString -> ShaderM s gt ()
logUniformArray len glType valuesFunc name =
    forM_ [0..len] $ \i -> do
        let fullName = name <> "[" <>
                (fromString $ show i) <> "]"
        logUniform glType (\x -> valuesFunc x !! i) fullName

declArrayNoLen :: (Reify t Type, Reify q Qualifier) =>
    [B.ByteString] -> Int -> Proxy q -> Proxy t -> B.ByteString ->
    ShaderM s gt (Array q t)
declArrayNoLen layouts len qualifier glType name = do
    let fullName = name <> "[]"
    ltell $ Decl (Layout layouts) (reify qualifier) (reify glType) fullName
    return $ Array name qualifier glType len

------------------------
-- Const Declarations --
------------------------

builtInArray ::
    Proxy t -> B.ByteString -> Int -> Array 'None t
builtInArray glType name len = Array name Proxy glType len

builtIn :: Proxy t -> B.ByteString -> Value 'None t
builtIn glType name = Value name nonep glType

ref :: Proxy t -> B.ByteString -> Value 'None t
ref = builtIn

constant :: Proxy t -> B.ByteString -> Expression t
constant glType name = Expression name glType

constNum :: (Num a, Show a) => Proxy t -> a -> Expression t
constNum glType value = Expression (fromString $ show value) glType

constInt :: Int -> Expression 'Int
constInt = constant (Proxy :: Proxy 'Int) . fromString . show

constFloat :: Float -> Expression 'Float
constFloat = constant (Proxy :: Proxy 'Float) . fromString . show

rawGLSL :: B.ByteString -> ShaderM s t ()
rawGLSL = ltell . Action

-----------------
-- Other utils --
-----------------

version :: B.ByteString -> ShaderM s t ()
version = ltell . Version

paren :: B.ByteString -> B.ByteString
paren s = "(" <> s <> ")"

fltd :: Float -> Float
fltd = id

intd :: Int -> Int
intd = id

ltell :: GLSLUnit -> ShaderM s t ()
ltell s = tell ([s], mempty)

rtell :: GLSLInfo t -> ShaderM s t ()
rtell s = tell ([], s)

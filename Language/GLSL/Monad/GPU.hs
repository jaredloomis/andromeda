{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
module Language.GLSL.Monad.GPU where

import Control.Applicative
import Control.Monad.Writer
import Control.Monad.RWS
import Data.Proxy
import Data.Vec (Vec2, Vec3, Vec4, Mat44)
import GHC.TypeLits

import qualified Data.ByteString as B

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL.Raw (GLfloat)

import Language.GLSL.Monad.Type

class GPU (a :: Type) where
    type CPU a :: *

    inConstr :: Proxy a -> (t -> [CPU a]) -> Name -> In t
    uniformConstr :: Proxy a -> (t -> CPU a) -> Name -> Uniform t
    outConstr :: Proxy a -> Name -> Out

    logIn :: Proxy a -> (t -> [CPU a]) -> Name -> ShaderM s t ()
    logIn proxy values = rtell . inInfo . inConstr proxy values
    logUniform :: Proxy a -> (t -> CPU a) -> Name -> ShaderM s t ()
    logUniform proxy value = rtell . uniformInfo . uniformConstr proxy value
    logOut :: Proxy a -> Name -> ShaderM s t ()
    logOut proxy = rtell . outInfo . outConstr proxy

instance GPU GInt where
    type CPU GInt = Int
    inConstr _ = InInt
    uniformConstr _ = UniformInt
    outConstr _ = OutInt

instance GPU GFloat where
    type CPU GFloat = GLfloat
    inConstr _ = InFloat
    uniformConstr _ = UniformFloat
    outConstr _ = OutFloat

instance GPU GVec2 where
    type CPU GVec2 = Vec2 GLfloat
    inConstr _ = InVec2
    uniformConstr _ = UniformVec2
    outConstr _ = OutVec2
instance GPU GVec3 where
    type CPU GVec3 = Vec3 GLfloat
    inConstr _ = InVec3
    uniformConstr _ = UniformVec3
    outConstr _ = OutVec3
instance GPU GVec4 where
    type CPU GVec4 = Vec4 GLfloat
    inConstr _ = InVec4
    uniformConstr _ = UniformVec4
    outConstr _ = OutVec4

instance GPU GMat4 where
    type CPU GMat4 = Mat44 GLfloat
    inConstr _ = InMat4
    uniformConstr _ = UniformMat4
    outConstr _ = OutMat4

instance GPU GSampler2D where
    type CPU GSampler2D = Sampler2D
    inConstr _ _ _ = error "ShaderM.inConstr: sampler2D cannot be an in var."
    uniformConstr _ = UniformSampler2D
    outConstr _ = OutSampler2D

instance GPU GSampler3D where
    type CPU GSampler3D = Sampler3D
    inConstr _ _ _ = error "ShaderM.inConstr: sampler3D cannot be an in var."
    uniformConstr _ = UniformSampler3D
    outConstr _ = OutSampler3D

instance (GPU a, GPU b) => GPU (GPair a b) where
    type CPU (GPair a b) = (CPU a, CPU b)
    inConstr _ valF n =
        let (asuf, bsuf) = pairSuffix
            pa = inConstr (Proxy :: Proxy a) (map fst . valF) (n <> asuf)
            pb = inConstr (Proxy :: Proxy b) (map snd . valF) (n <> bsuf)
        in InPair pa pb
    uniformConstr _ valF n =
        let (asuf, bsuf) = pairSuffix
            pa = uniformConstr (Proxy :: Proxy a) (fst . valF) (n <> asuf)
            pb = uniformConstr (Proxy :: Proxy b) (snd . valF) (n <> bsuf)
        in UniformPair pa pb
    outConstr _ n =
        let (asuf, bsuf) = pairSuffix
            pa = outConstr (Proxy :: Proxy a) (n <> asuf)
            pb = outConstr (Proxy :: Proxy b) (n <> bsuf)
        in OutPair pa pb

instance GPU GNoType where
    type CPU GNoType = ()
    inConstr _ _ = InNone 
    uniformConstr _ _ _ =
        error "ShaderM.uniformConstr: notype cannot be an uniform."
    outConstr _ = OutNone

type VertexShaderM t a = ShaderM GL.VertexShader t a
type GeometryShaderM t a = ShaderM GL.GeometryShader t a
type FragmentShaderM t a = ShaderM GL.FragmentShader t a

newtype ShaderM (s :: GL.ShaderType) t a = ShaderM {
    sInnerShaderM :: RWS () ([GLSLUnit], GLSLInfo t) ShaderState a
    } deriving (Functor, Applicative, Monad)

data ShaderState = ShaderState {
    shStateVarCount :: Int
    }

incrementVars :: ShaderM s t ()
incrementVars =
    modify (\st -> st{shStateVarCount = shStateVarCount st + 1})

ltell :: GLSLUnit -> ShaderM s t ()
ltell s = tell ([s], mempty)

rtell :: GLSLInfo t -> ShaderM s t ()
rtell s = tell ([], s)

instance MonadWriter ([GLSLUnit], GLSLInfo t) (ShaderM s t) where
    listen (ShaderM glsl) = ShaderM $ listen glsl
    pass (ShaderM glsl) = ShaderM $ pass glsl
    tell = ShaderM . tell
    writer = ShaderM . writer

instance MonadState ShaderState (ShaderM s t) where
    state = ShaderM . state

data GLSLUnit where
    Version :: B.ByteString -> GLSLUnit
    Decl :: Layout -> Qualifier -> Type -> Name -> GLSLUnit
    AssignStatement :: Type -> Name -> B.ByteString -> GLSLUnit
--  | AssignStatement Name B.ByteString
    Action :: B.ByteString -> GLSLUnit

newtype Layout = Layout [B.ByteString]
  deriving (Show, Eq, Monoid)

-- = Type-level to B.ByteString stuff.

class Reify a b | a -> b where
    reify :: Proxy a -> b

instance Reify GInt Type where
    reify _ = GInt
instance Reify GUInt Type where
    reify _ = GUInt
instance Reify GFloat Type where
    reify _ = GFloat
instance Reify GMat4 Type where
    reify _ = GMat4
instance Reify GVec4 Type where
    reify _ = GVec4
instance Reify GVec3 Type where
    reify _ = GVec3
instance Reify GVec2 Type where
    reify _ = GVec2
instance Reify GSampler2D Type where
    reify _ = GSampler2D
instance Reify GSampler3D Type where
    reify _ = GSampler3D
instance (Reify a Type, Reify b Type) => Reify (GPair a b) Type where
    reify _ = GPair (reify (Proxy :: Proxy a)) (reify (Proxy :: Proxy b))
instance Reify GNoType Type where
    reify _ = GNoType

instance Reify 'In Qualifier where
    reify _ = In
instance Reify 'Out Qualifier where
    reify _ = Out
instance Reify 'Uniform Qualifier where
    reify _ = Uniform
instance Reify 'None Qualifier where
    reify _ = None

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

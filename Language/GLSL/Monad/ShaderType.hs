{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
--{-# LANGUAGE NoMonomorphismRestriction #-}
module Language.GLSL.Monad.ShaderType where

import Data.Proxy

import qualified Data.ByteString as B

import qualified Graphics.Rendering.OpenGL as GL

import Language.GLSL.Monad.Type
import Language.GLSL.Monad.GPU

-- TODO: Add Compute shader for general purpose
--       computing on the GPU.

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

{- -- TODO: This would be nice.
inn' :: (ShaderType s, GPU t, Reify t Type) =>
        InArgs s gt t -> ShaderM s gt (Value 'In t)
inn' = inn Proxy
-}

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

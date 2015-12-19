{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
module Andromeda.Simple.Render.Mesh where

import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (Storable(..))

import qualified Data.Vector.Storable as V

import qualified Graphics.Rendering.OpenGL.GL as GL

import Andromeda.Simple.Type
import Andromeda.Simple.Render.VertexBuffer

data Mesh = Mesh [(String, MeshAttribute)] !Primitive

data MeshAttribute where
    MeshAttribute :: (Typed a, Storable a) =>
        !(V.Vector a) -> MeshAttribute
    MeshUniform   :: Uniform -> MeshAttribute

data CompiledMesh = CompiledMesh
    [(String, VertexBuffer)]
    [(String, Uniform)]
    !Primitive

data Uniform where
    Uniform :: (Storable a, GL.Uniform a) =>
               a -> Uniform

compileMesh :: Mesh -> IO CompiledMesh
compileMesh (Mesh attrs prim) =
    CompiledMesh <$>         compileAttrs attrs
                 <*> return (compileUnifs attrs)
                 <*> return prim
  where
    compileAttrs ((name, attr@MeshAttribute{}) : xs) = do
        attr' <- (name,) <$> toBuffer attr
        (attr' :) <$> compileAttrs xs
    compileAttrs (_ : xs) = compileAttrs xs
    compileAttrs [] = return []

    compileUnifs ((name, MeshUniform unif) : xs) =
        (name, unif) : compileUnifs xs
    compileUnifs (_ : xs) = compileUnifs xs
    compileUnifs [] = []

toBuffer :: MeshAttribute -> IO VertexBuffer
toBuffer (MeshAttribute xs) =
    let len = fromIntegral $ V.length xs
        nullPtr' = helpInference xs nullPtr
        descriptor = GL.VertexArrayDescriptor len GL.Float 0 nullPtr'
    in do
        buffer <- makeBuffer GL.ArrayBuffer xs
        return $ VertexBuffer buffer descriptor
  where
    helpInference :: V.Vector a -> Ptr a -> Ptr a
    helpInference _ ptr = ptr
toBuffer MeshUniform{} = error "toBuffer recieved MeshUniform"

replaceBuffer :: MeshAttribute -> VertexBuffer -> IO VertexBuffer
replaceBuffer (MeshAttribute xs)
          buf@(VertexBuffer buffer (GL.VertexArrayDescriptor len _ _ _)) = do
    GL.bindBuffer GL.ArrayBuffer GL.$= Just buffer
    replaceBuffer' GL.ArrayBuffer xs (fromIntegral len)
    return buf
replaceBuffer MeshUniform{} _ = error "replaceBuffer recieved MeshUniform"

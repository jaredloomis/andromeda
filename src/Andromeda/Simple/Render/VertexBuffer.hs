{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Andromeda.Simple.Render.VertexBuffer where

import Foreign.Storable (Storable(..))

import qualified Data.Vector.Storable as V

import qualified Graphics.Rendering.OpenGL.GL as GL

data VertexBuffer where
    VertexBuffer :: Storable a =>
        !GL.BufferObject ->
        !(GL.VertexArrayDescriptor a) ->
        VertexBuffer

data Primitive = Points | TriangleStrip | Triangles
  deriving (Show, Eq)

bindBuffer :: VertexBuffer -> GL.AttribLocation -> IO ()
bindBuffer (VertexBuffer buffer descriptor) location = do
    GL.vertexAttribArray location GL.$= GL.Enabled
    GL.bindBuffer GL.ArrayBuffer GL.$= Just buffer
    GL.vertexAttribPointer location GL.$= (GL.ToFloat, descriptor)

bufferLen :: VertexBuffer -> GL.GLint
bufferLen (VertexBuffer _ (GL.VertexArrayDescriptor n _ _ _)) = n

makeBuffer :: forall a. (Storable a) =>
    GL.BufferTarget -> V.Vector a -> IO GL.BufferObject
makeBuffer target elems = do
    arrayBuffer <- GL.genObjectName
    GL.bindBuffer target GL.$= Just arrayBuffer

    V.unsafeWith elems $ \ptr ->
        let n = fromIntegral $ V.length elems *
                sizeOf (error "makeBuffer" :: a)
        in GL.bufferData target GL.$= (n, ptr, GL.StaticDraw)

    return arrayBuffer

replaceBuffer' :: forall a. Storable a =>
    GL.BufferTarget -> V.Vector a -> Int -> IO ()
replaceBuffer' target elems len =
    V.unsafeWith elems $ \ptr -> do
        let dataSize = fromIntegral $ len *
                sizeOf (error "replaceBuffer" :: a)
        GL.bufferData target GL.$= (dataSize, ptr, GL.StaticDraw)


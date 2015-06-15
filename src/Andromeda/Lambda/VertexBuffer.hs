{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
module Andromeda.Lambda.VertexBuffer where

import GHC.TypeLits (KnownNat)
import GHC.Stack (errorWithStackTrace)
import Foreign.Storable
import Foreign.Marshal.Array (withArrayLen, withArray)
import Foreign.Ptr (nullPtr)
import Unsafe.Coerce (unsafeCoerce)

import Data.Vec ((:.)(..), Vec2, Vec3)

import qualified Graphics.Rendering.OpenGL.GL as GL

import Andromeda.Lambda.Glom
import Andromeda.Lambda.Type
import Andromeda.Lambda.Utils

type AttrGlom a = Glom VertexBuffer' a

data VertexBuffer' a where
    VertexBuffer' :: Storable a =>
                GL.BufferObject ->
                GL.AttribLocation ->
                GL.VertexArrayDescriptor a ->
                VertexBuffer' a
    UniformPrim :: HasVertex a =>
        a -> GL.UniformLocation -> VertexBuffer' a

data Input a where
    InInput      :: [a] -> Input a
    UniformInput :: a -> Input a
    LamI         :: Input a -> Input b -> Input (a -> b)
    PairI        :: Input a -> Input b -> Input (a, b)

----

data Input' where
    InIn   :: HasVertex a => [a] -> Type a -> String -> Input'
    InUnif :: HasVertex a =>  a  -> Type a -> String -> Input'

data VertexBuffer = forall a. HasVertex a => VertexBuffer (AttrGlom a)

toVertex' :: Input' -> GL.Program -> IO VertexBuffer
toVertex' (InIn  xs _ n) prog =
    VertexBuffer <$> toVertex (InInput xs) (pat n) prog
toVertex' (InUnif x _ n) prog =
    VertexBuffer <$> toVertex (UniformInput x) (pat n) prog

bindVertex' :: VertexBuffer -> IO ()
bindVertex' (VertexBuffer attr) = bindVertex attr

replaceVertex' :: Input' -> VertexBuffer -> IO VertexBuffer
replaceVertex' (InIn  xs _ _) (VertexBuffer attr) =
    VertexBuffer <$> replaceVertex (InInput xs) (unsafeCoerce attr)
replaceVertex' (InUnif x _ _) (VertexBuffer attr) =
    VertexBuffer <$> replaceVertex (UniformInput x) (unsafeCoerce attr)

----

class HasType a => HasVertex a where
    toVertex      :: Input a -> Pat a -> GL.Program -> IO (AttrGlom a)
    bindVertex    :: AttrGlom a -> IO ()
    replaceVertex :: Input a -> AttrGlom a -> IO (AttrGlom a)

instance HasVertex Float where
    toVertex (InInput xs) (BaseG (V name _)) prog =
        toVertexBuffer' xs GL.Float name prog
    toVertex (UniformInput x) (BaseG (V name _)) prog =
        toVertexUnif x name prog

    bindVertex (BaseG (VertexBuffer' buffer location descriptor)) =
        bindVertexBuffer' buffer location descriptor
    bindVertex (BaseG (UniformPrim x location)) =
        bindVertexUnif location $ GL.Index1 (unsafeCoerce x :: GL.GLfloat)

    replaceVertex (InInput xs)
           attr@(BaseG (VertexBuffer' buffer _ desc)) = do
        replaceVertexBuffer' xs buffer desc
        return attr
    replaceVertex (UniformInput x) (BaseG (UniformPrim _ loc)) =
        return $ BaseG (UniformPrim x loc)
    replaceVertex _ _ = errorWithStackTrace $
        "replaceVertex given input and attr" ++
        "with different qualifiers."

instance HasVertex (Vec2 Float) where
    toVertex (InInput xs) (BaseG (V name _)) prog =
        toVertexBuffer' xs GL.Float name prog
    toVertex (UniformInput x) (BaseG (V name _)) prog =
        toVertexUnif x name prog

    bindVertex (BaseG (VertexBuffer' buffer location descriptor)) =
        bindVertexBuffer' buffer location descriptor
    bindVertex (BaseG (UniformPrim (x:.y:.()) location)) =
        bindVertexUnif location $ GL.Vertex2
            (unsafeCoerce x :: GL.GLfloat)
            (unsafeCoerce y :: GL.GLfloat)

    replaceVertex (InInput xs)
           attr@(BaseG (VertexBuffer' buffer _ desc)) = do
        replaceVertexBuffer' xs buffer desc
        return attr
    replaceVertex (UniformInput x) (BaseG (UniformPrim _ loc)) =
        return $ BaseG (UniformPrim x loc)
    replaceVertex _ _ = errorWithStackTrace $
        "replaceVertex given input and attr" ++
        "with different qualifiers."

instance HasVertex (Vec3 Float) where
    toVertex (InInput xs) (BaseG (V name _)) prog =
        toVertexBuffer' xs GL.Float name prog
    toVertex (UniformInput x) (BaseG (V name _)) prog =
        toVertexUnif x name prog

    bindVertex (BaseG (VertexBuffer' buffer location descriptor)) =
        bindVertexBuffer' buffer location descriptor
    bindVertex (BaseG (UniformPrim (x:.y:.z:.()) location)) =
        bindVertexUnif location $ GL.Vertex3
            (unsafeCoerce x :: GL.GLfloat)
            (unsafeCoerce y :: GL.GLfloat)
            (unsafeCoerce z :: GL.GLfloat)

    replaceVertex (InInput xs)
           attr@(BaseG (VertexBuffer' buffer _ desc)) = do
        replaceVertexBuffer' xs buffer desc
        return attr
    replaceVertex (UniformInput x) (BaseG (UniformPrim _ loc)) =
        return $ BaseG (UniformPrim x loc)
    replaceVertex _ _ = errorWithStackTrace $
        "replaceVertex given input and attr" ++
        "with different qualifiers."

instance (HasVertex a, HasVertex b) => HasVertex (a, b) where
    toVertex (PairI pa pb) (PairG na nb) prog = do
        attra <- toVertex pa na prog
        attrb <- toVertex pb nb prog
        return $ attra `PairG` attrb
    toVertex _ _ _ = errorWithStackTrace $
        "toVertex (a, b): given pair constructed" ++
        " in some unknown way."

    bindVertex (PairG la lb) = bindVertex la >> bindVertex lb
    bindVertex _ = errorWithStackTrace $
        "bindVertex (a, b): given pair constructed" ++
        " in some unknown way."

    replaceVertex (PairI li ri) (PairG lg rg) = do
        attrl <- replaceVertex li lg
        attrr <- replaceVertex ri rg
        return $ PairG attrl attrr
    replaceVertex _ _ = errorWithStackTrace $
        "replaceVertex (a, b): given pair constructed" ++
        " in some unknown way."

instance KnownNat n => HasVertex (Sampler n) where
    toVertex = undefined
    bindVertex = undefined
    replaceVertex = undefined

-- TODO XXX: More instances of HasVertex.

-----------------------------------
-- Helpers for HasVertex instances --
-----------------------------------

toVertexBuffer' :: (Storable a, Storable b, HasType b) =>
    [a] -> GL.DataType -> String -> GL.Program -> IO (AttrGlom b)
toVertexBuffer' xs glType name prog =
    let len = fromIntegral $ length xs
        descriptor = GL.VertexArrayDescriptor len glType 0 nullPtr
    in do
        buffer <- makeBuffer GL.ArrayBuffer xs
        location <- GL.get $ GL.attribLocation prog name
        return $ BaseG (VertexBuffer' buffer location descriptor)

toVertexUnif :: HasVertex a => a -> String -> GL.Program -> IO (AttrGlom a)
toVertexUnif x name prog = do
    location <- GL.get $ GL.uniformLocation prog name
    return $ BaseG (UniformPrim x location)

bindVertexBuffer' :: GL.BufferObject -> GL.AttribLocation ->
                GL.VertexArrayDescriptor a -> IO ()
bindVertexBuffer' buffer location descriptor = do
    GL.vertexAttribArray location GL.$= GL.Enabled
    GL.bindBuffer GL.ArrayBuffer GL.$= Just buffer
    GL.vertexAttribPointer location GL.$= (GL.ToFloat, descriptor)

bindVertexUnif :: GL.Uniform a => GL.UniformLocation -> a -> IO ()
bindVertexUnif location unif =
    GL.uniform location GL.$= unif

replaceVertexBuffer' :: Storable a =>
    [a] -> GL.BufferObject -> GL.VertexArrayDescriptor b -> IO ()
replaceVertexBuffer' xs buffer (GL.VertexArrayDescriptor len _ _ _) = do
    GL.bindBuffer GL.ArrayBuffer GL.$= Just buffer
    replaceBuffer GL.ArrayBuffer xs (fromIntegral len)

makeBuffer :: forall a. (Storable a) =>
    GL.BufferTarget -> [a] -> IO GL.BufferObject
makeBuffer target elems = do
    arrayBuffer <- GL.genObjectName

    GL.bindBuffer target GL.$= Just arrayBuffer

    withArrayLen elems $ \len ptr ->
        let n = fromIntegral $ len *
                sizeOf (errorWithStackTrace "makeBuffer" :: a)
        in GL.bufferData target GL.$= (n, ptr, GL.StaticDraw)

    return arrayBuffer

replaceBuffer :: forall a. Storable a =>
    GL.BufferTarget -> [a] -> Int -> IO ()
replaceBuffer target elems len =
    withArray elems $ \ptr -> do
        let dataSize = fromIntegral $ len *
                sizeOf (errorWithStackTrace "replaceBuffer" :: a)
        GL.bufferData target GL.$= (dataSize, ptr, GL.StaticDraw)

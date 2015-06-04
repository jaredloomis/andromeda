{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
module Andromeda.Lambda.HasAttr where

import GHC.TypeLits (KnownNat)
import GHC.Stack (errorWithStackTrace)
import Foreign.Storable
import Foreign.Marshal.Array (withArrayLen, withArray)
import Foreign.Ptr (nullPtr)
import Data.String (fromString)
import Control.Monad.State
import Debug.Trace (trace)
import Unsafe.Coerce (unsafeCoerce)
import System.Exit (exitFailure)

import Data.Vec ((:.)(..), Vec2, Vec3, Vec4)

import qualified Graphics.Rendering.OpenGL.GL as GL

import Andromeda.Lambda.Glom
import Andromeda.Lambda.Type
import Andromeda.Lambda.Utils

type Attr a = Glom AttrPrim a

data AttrPrim a where
    AttrPrim :: Storable a =>
                GL.BufferObject ->
                GL.AttribLocation ->
                GL.VertexArrayDescriptor a ->
                AttrPrim a
    UniformPrim :: HasAttr a =>
        a -> GL.UniformLocation -> AttrPrim a

data Input a where
    InInput      :: [a] -> Input a
    UniformInput :: a -> Input a
    LamI         :: Input a -> Input b -> Input (a -> b)
    PairI        :: Input a -> Input b -> Input (a, b)

data Input' where
    InIn   :: HasAttr a => [a] -> Type a -> String -> Input'
    InUnif :: HasAttr a =>  a  -> Type a -> String -> Input'

class HasType a => HasAttr a where
    toAttr      :: Input a -> Pat a -> GL.Program -> IO (Attr a)
    bindAttr    :: Attr a -> IO ()
    replaceAttr :: Input a -> Attr a -> IO (Attr a)

instance HasAttr Float where
    toAttr (InInput xs) (BaseG (V name _)) prog =
        toAttrPrim xs GL.Float name prog
    toAttr (UniformInput x) (BaseG (V name _)) prog =
        toAttrUnif x name prog

    bindAttr (BaseG (AttrPrim buffer location descriptor)) =
        bindAttrPrim buffer location descriptor
    bindAttr (BaseG (UniformPrim x location)) =
        bindAttrUnif location $ GL.Index1 (unsafeCoerce x :: GL.GLfloat)

    replaceAttr (InInput xs)
           attr@(BaseG (AttrPrim buffer _ desc)) = do
        replaceAttrPrim xs buffer desc
        return attr
    replaceAttr (UniformInput x) (BaseG (UniformPrim _ loc)) =
        return $ BaseG (UniformPrim x loc)
    replaceAttr _ _ = errorWithStackTrace $
        "replaceAttr given input and attr" ++
        "with different qualifiers."

instance HasAttr (Vec2 Float) where
    toAttr (InInput xs) (BaseG (V name _)) prog =
        toAttrPrim xs GL.Float name prog
    toAttr (UniformInput x) (BaseG (V name _)) prog =
        toAttrUnif x name prog

    bindAttr (BaseG (AttrPrim buffer location descriptor)) =
        bindAttrPrim buffer location descriptor
    bindAttr (BaseG (UniformPrim (x:.y:.()) location)) =
        bindAttrUnif location $ GL.Vertex2
            (unsafeCoerce x :: GL.GLfloat)
            (unsafeCoerce y :: GL.GLfloat)

    replaceAttr (InInput xs)
           attr@(BaseG (AttrPrim buffer _ desc)) = do
        replaceAttrPrim xs buffer desc
        return attr
    replaceAttr (UniformInput x) (BaseG (UniformPrim _ loc)) =
        return $ BaseG (UniformPrim x loc)
    replaceAttr _ _ = errorWithStackTrace $
        "replaceAttr given input and attr" ++
        "with different qualifiers."

instance HasAttr (Vec3 Float) where
    toAttr (InInput xs) (BaseG (V name _)) prog =
        toAttrPrim xs GL.Float name prog
    toAttr (UniformInput x) (BaseG (V name _)) prog =
        toAttrUnif x name prog

    bindAttr (BaseG (AttrPrim buffer location descriptor)) =
        bindAttrPrim buffer location descriptor
    bindAttr (BaseG (UniformPrim (x:.y:.z:.()) location)) =
        bindAttrUnif location $ GL.Vertex3
            (unsafeCoerce x :: GL.GLfloat)
            (unsafeCoerce y :: GL.GLfloat)
            (unsafeCoerce z :: GL.GLfloat)

    replaceAttr (InInput xs)
           attr@(BaseG (AttrPrim buffer _ desc)) = do
        replaceAttrPrim xs buffer desc
        return attr
    replaceAttr (UniformInput x) (BaseG (UniformPrim _ loc)) =
        return $ BaseG (UniformPrim x loc)
    replaceAttr _ _ = errorWithStackTrace $
        "replaceAttr given input and attr" ++
        "with different qualifiers."

instance (HasAttr a, HasAttr b) => HasAttr (a, b) where
    toAttr (PairI pa pb) (PairG na nb) prog = do
        attra <- toAttr pa na prog
        attrb <- toAttr pb nb prog
        return $ attra `PairG` attrb
    toAttr _ _ _ = errorWithStackTrace $
        "toAttr (a, b): given pair constructed" ++
        " in some unknown way."

    bindAttr (PairG la lb) = bindAttr la >> bindAttr lb
    bindAttr _ = errorWithStackTrace $
        "bindAttr (a, b): given pair constructed" ++
        " in some unknown way."

    replaceAttr (PairI li ri) (PairG lg rg) = do
        attrl <- replaceAttr li lg
        attrr <- replaceAttr ri rg
        return $ PairG attrl attrr
    replaceAttr _ _ = errorWithStackTrace $
        "replaceAttr (a, b): given pair constructed" ++
        " in some unknown way."

instance KnownNat n => HasAttr (Sampler n) where
    toAttr = undefined
    bindAttr = undefined
    replaceAttr = undefined

-- TODO XXX: More instances of HasAttr.

-----------------------------------
-- Helpers for HasAttr instances --
-----------------------------------

toAttrPrim :: (Storable a, Storable b, HasType b) =>
    [a] -> GL.DataType -> String -> GL.Program -> IO (Attr b)
toAttrPrim xs glType name prog =
    let len = fromIntegral $ length xs
        descriptor = GL.VertexArrayDescriptor len glType 0 nullPtr
    in do
        buffer <- makeBuffer GL.ArrayBuffer xs
        location <- GL.get $ GL.attribLocation prog name
        return $ BaseG (AttrPrim buffer location descriptor)

toAttrUnif :: HasAttr a => a -> String -> GL.Program -> IO (Attr a)
toAttrUnif x name prog = do
    location <- GL.get $ GL.uniformLocation prog name
    return $ BaseG (UniformPrim x location)

bindAttrPrim :: GL.BufferObject -> GL.AttribLocation ->
                GL.VertexArrayDescriptor a -> IO ()
bindAttrPrim buffer location descriptor = do
    GL.vertexAttribArray location GL.$= GL.Enabled
    GL.bindBuffer GL.ArrayBuffer GL.$= Just buffer
    GL.vertexAttribPointer location GL.$= (GL.ToFloat, descriptor)

bindAttrUnif :: GL.Uniform a => GL.UniformLocation -> a -> IO ()
bindAttrUnif location unif =
    GL.uniform location GL.$= unif

replaceAttrPrim :: Storable a =>
    [a] -> GL.BufferObject -> GL.VertexArrayDescriptor b -> IO ()
replaceAttrPrim xs buffer (GL.VertexArrayDescriptor len _ _ _) = do
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

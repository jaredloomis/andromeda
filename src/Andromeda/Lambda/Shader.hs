{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
module Andromeda.Lambda.Shader where

import GHC.Stack (errorWithStackTrace)
import Foreign.Storable
import Foreign.Marshal.Array (withArrayLen, withArray)
import Foreign.Ptr (nullPtr)
import Data.String (fromString)
import Data.Word (Word, Word8)
import Control.Applicative (Applicative)
import Control.Monad.State
import Debug.Trace (trace)
import Unsafe.Coerce (unsafeCoerce)

import Data.Vec ((:.)(..), Vec4, Vec3, Vec2)

import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.UI.GLFW as GLFW

import Andromeda.Lambda.Expr
import Andromeda.Lambda.Type
import Andromeda.Lambda.Glom
import Andromeda.Lambda.GLSL
--import Andromeda.Lambda.Utils
    
data Input a where
    InInput      :: [a] -> Input a
    UniformInput :: a -> Input a
    PairI        :: Input a -> Input b -> Input (a, b)

type Attr a = Glom AttrPrim a

data AttrPrim a where
    AttrPrim :: Storable a =>
                GL.BufferObject ->
                GL.AttribLocation ->
                GL.VertexArrayDescriptor a ->
                AttrPrim a
    UniformPrim :: HasAttr a =>
        a -> GL.UniformLocation -> AttrPrim a

-------------
-- HasAttr --
-------------

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
    bindAttr (BaseG (UniformPrim x location)) = do
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

instance HasAttr (Vec3 Float) where
    toAttr (InInput xs) (BaseG (V name _)) prog =
        toAttrPrim xs GL.Float name prog
    toAttr (UniformInput x) (BaseG (V name _)) prog =
        toAttrUnif x name prog

    bindAttr (BaseG (AttrPrim buffer location descriptor)) =
        bindAttrPrim buffer location descriptor
    bindAttr (BaseG (UniformPrim (x:.y:.z:.()) location)) = do
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

---------------------
-- Running program --
---------------------

mainLoop :: HasAttr i =>
    GLFW.Window -> Program g i ->
    g -> (g -> g) -> IO ()
mainLoop win prog g update = do
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]
    prog' <- drawProgram prog g

    endFrame
    shouldClose <- GLFW.windowShouldClose win
    unless shouldClose $
        mainLoop win prog' (update g) update
  where
    endFrame = do
        GLFW.swapBuffers win
        GLFW.pollEvents


drawProgram :: HasAttr i => Program g i -> g -> IO (Program g i)
drawProgram (Program prog len attr update) g = do
    GL.currentProgram GL.$= Just prog

    let input = update g
    attr' <- replaceAttr input attr
    bindAttr attr'

    GL.drawArrays GL.Triangles 0 $ fromIntegral len
    GL.bindBuffer GL.ArrayBuffer GL.$= Nothing
    GL.currentProgram GL.$= Nothing

    return $ Program prog len attr update

------------------------
-- Compile to program --
------------------------

data Program g i =
    Program
        GL.Program
        Int
        (Attr i)
        (g -> Input i)

version :: String
version = "#version 330 core\n"

inputLen :: Input a -> Int
inputLen (InInput xs) = length xs
inputLen (UniformInput _) = 1
inputLen (PairI a b) = max (inputLen a) (inputLen b)

compile :: forall i m o g. (HasAttr i, HasType o, HasType m, HasGLSL m) =>
    Expr (i -> (Vec4 Float, m)) -> Expr (m -> o) ->
    Input i -> (g -> Input i) ->
    IO (Program g i)
compile vert frag input update =
    let len = inputLen input
        inPat = pat "prim_input_var" :: Pat i
        quals = qualifiers input
        vStr  = compileVert vert quals
        fStr  = compileFrag frag
    in do
        prog <- compileAndLink vStr fStr
        attr <- toAttr input inPat prog
        return $ Program prog len attr update

data Quals = BaseQ Qualifier | PairQ Quals Quals

qualifiers :: Input i -> Quals
qualifiers (InInput _) = BaseQ In
qualifiers (UniformInput _) = BaseQ Uniform
qualifiers (PairI l r) = PairQ (qualifiers l) (qualifiers r)

compileVert :: forall i o. (HasType i, HasType o, HasGLSL o) =>
    Expr (i -> (Vec4 Float, o)) -> Quals -> String
compileVert expr quals =
    let inPat   = pat "prim_input_var"     :: Pat i
        outPat  = pat "prim_vert2frag_var" :: Pat o
        inDefV  = defineTopQ quals inPat
        outDefV = defineTop Out outPat
        apExprV = expr :$ Var (V "prim_input_var" $ typeOf (undefined :: i))
        (exprVA, exprVB) = unPair apExprV
        vertStm = (outPat =: exprVB) >> (pat "gl_Position" =: exprVA)
        mainV   = Definition Nothing "main" vertStm
        vStr    = version ++ inDefV ++ outDefV ++ toGLSL mainV
    in trace vStr vStr

compileFrag :: forall i o. (HasType i, HasType o) =>
    Expr (i -> o) -> String
compileFrag expr =
    let inPat   =  pat "prim_vert2frag_var" :: Pat i
        outPat  =  pat "prim_output_var"    :: Pat o
        inDefF  = defineTop In inPat
        outDefF = defineTop Out outPat
        apExprF = expr :$ Var (V "prim_vert2frag_var" $ typeOf (undefined :: i))
        mainF   = Definition Nothing "main" (outPat =: apExprF)
        fStr    = version ++ inDefF ++ outDefF ++ toGLSL mainF
    in trace fStr fStr

defineTop :: Qualifier -> Pat a -> String
defineTop q (l `PairG` r) = defineTop q l ++ defineTop q r
defineTop q (BaseG (V n t)) = toGLSL q++" "++toGLSL t++" "++n++";\n"
defineTop _ UnitG = ""

defineTopQ :: Quals -> Pat i -> String
defineTopQ (lq `PairQ` rq) (l `PairG` r) = defineTopQ lq l ++ defineTopQ rq r
defineTopQ (BaseQ q) (BaseG (V n t)) = toGLSL q++" "++toGLSL t++" "++n++";\n"
defineTopQ _ UnitG = ""
defineTopQ _ _ = errorWithStackTrace "defineTopQ: Quals do not match Pat."

----------------------------------------------------
-- Functions to compile shaders to a 'GL.Program' --
----------------------------------------------------

compileAndLink :: String -> String -> IO GL.Program
compileAndLink vert frag = do
    program <- GL.createProgram

    v <- compileGLSL vert GL.VertexShader
    GL.attachShader program v
    f <- compileGLSL frag GL.FragmentShader
    GL.attachShader program f

    GL.linkProgram program

    return program

compileGLSL :: String -> GL.ShaderType -> IO GL.Shader
compileGLSL src shaderType = do
    shader <- GL.createShader shaderType
    GL.shaderSourceBS shader GL.$= fromString src
    GL.compileShader shader
    -- Check status.
    ok <- GL.get $ GL.compileStatus shader
    unless ok $
        print =<< GL.get (GL.shaderInfoLog shader)

    return shader

------------------
-- Buffer utils --
------------------

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

----------------
-- GLFW Stuff --
----------------

openWindow :: IO GLFW.Window
openWindow = do
    _ <- GLFW.init

    -- Give GLFW some hints.
    mapM_ GLFW.windowHint
        [GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core,
         GLFW.WindowHint'DepthBits 16,
         GLFW.WindowHint'Samples 4,
         GLFW.WindowHint'Resizable True,
         GLFW.WindowHint'ClientAPI GLFW.ClientAPI'OpenGL,
         GLFW.WindowHint'ContextVersionMajor 4,
         GLFW.WindowHint'ContextVersionMinor 3]

    -- Open window.
    Just win <- GLFW.createWindow
                    800
                    600
                    "GLFW Window" Nothing Nothing
    GLFW.makeContextCurrent (Just win)

    -- Set resize callback.
    GLFW.setFramebufferSizeCallback win (Just resizeScene)

    -- Enable VSync
    GLFW.swapInterval 1

    return win

initGL :: GLFW.Window -> IO ()
initGL win = do
    -- Set "background color" to black
    GL.clearColor GL.$= GL.Color4 0 0 0 1

    -- Enables clearing of the depth buffer
    GL.clearDepth GL.$= 1
    -- Allow depth testing (3D)
    -- Tells OpenGL how to deal with overlapping shapes
    GL.depthFunc GL.$= Just GL.Less

    -- Do not render the backs of faces. Increases performance.
    GL.cullFace GL.$= Just GL.Back

    -- Call resize function.
    (w, h) <- GLFW.getFramebufferSize win
    resizeScene win w h 

resizeScene :: GLFW.WindowSizeCallback
-- Prevent divide by 0
resizeScene win w 0 = resizeScene win w 1
resizeScene _ width height =
    -- Make viewport the same size as the window.
    GL.viewport GL.$= (GL.Position 0 0,
                    GL.Size (fromIntegral width) (fromIntegral height))

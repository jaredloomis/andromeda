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
module Shader where

import Foreign.Storable
import Foreign.Marshal.Array (withArrayLen, withArray)
import Foreign.Ptr (nullPtr)
import Data.Monoid (Monoid(..), (<>))
import Control.Monad.Identity (Identity(..))
import Control.Monad (unless)
import Data.String (fromString)
import Data.Word (Word, Word8)
import Control.Applicative (Applicative)
import Control.Monad.State

import Data.Vec ((:.)(..), Vec4, Vec3, Vec2)

import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.UI.GLFW as GLFW

import Expr
import Type
import Glom
import GLSL
import HasGLSL

{-
newtype Shader a = Shader {
    runShader :: State (Free Statement) a
    } deriving (Functor, Applicative, Monad)

(%=) :: HasGLSL a => Pat a -> Expr a -> Shader ()
(%=) patt expr = Shader $ put $ Assign (Bind patt expr)
-}

newtype Change s a = Change {
    runChange :: State (s, Bool) a
    } deriving (Functor, Applicative, Monad)

instance MonadState s (Change s) where
    get = Change (fst `fmap` get)
    put s = Change $ put (s, True)

-------------------
-- GPU typeclass --
-------------------

class HasType a => GPU a where
    toAttr :: [a] -> Pat a -> GL.Program -> IO (Attr a)
--    toUniform :: a -> Pat a -> GL.Progr

-- Scalars
instance GPU Float where
    toAttr xs (BaseG (V name _)) =
        toAttrPrim xs GL.Float name
instance GPU Int where
    toAttr xs (BaseG (V name _)) =
        toAttrPrim xs GL.Int name
instance GPU Word where
    toAttr xs (BaseG (V name _)) =
        toAttrPrim xs GL.UnsignedInt name
instance GPU Bool where
    toAttr xs (BaseG (V name _)) =
        toAttrPrim (map boolToWord8 xs) GL.UnsignedByte name
      where
        boolToWord8 :: Bool -> Word8
        boolToWord8 True  = 1
        boolToWord8 False = 0
-- Vecs
instance (KnownScalar a, Storable a) => GPU (Vec2 a) where
    toAttr xs (BaseG (V name _)) =
        toAttrPrim xs (glScalarType (undefined :: a)) name
instance (KnownScalar a, Storable a) => GPU (Vec3 a) where
    toAttr xs (BaseG (V name _)) =
        toAttrPrim xs (glScalarType (undefined :: a)) name
instance (KnownScalar a, Storable a) => GPU (Vec4 a) where
    toAttr xs (BaseG (V name _)) =
        toAttrPrim xs (glScalarType (undefined :: a)) name

toAttrPrim :: Storable a =>
    [a] -> GL.DataType -> String -> GL.Program -> IO (Attr b)
toAttrPrim xs glType name prog =
    let len = fromIntegral $ length xs
        descriptor = GL.VertexArrayDescriptor len glType 0 nullPtr
    in do
        buffer <- makeBuffer GL.ArrayBuffer xs
        location <- GL.get $ GL.attribLocation prog name
        return $ Attr buffer location descriptor

-- Running a compiled Program --

mainLoop :: GLFW.Window -> Program -> IO ()
mainLoop win prog = do
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]
    drawProgram prog

    endFrame
    shouldClose <- GLFW.windowShouldClose win
    unless shouldClose $
        mainLoop win prog
  where
    endFrame = do
        GLFW.swapBuffers win
        GLFW.pollEvents

drawProgram :: Program -> IO ()
drawProgram (Program prog bindInVars len) = do
    -- Use shader program.
    GL.currentProgram GL.$= Just prog

    -- Bind in vars.
    bindInVars

    -- Draw.
    GL.drawArrays GL.Triangles 0 $ fromIntegral len

    -- Unbind in vars.
    GL.bindBuffer GL.ArrayBuffer GL.$= Nothing
    -- Turn off shader.
    GL.currentProgram GL.$= Nothing

-- Compiling --

data ShaderFunction a = ShaderFunction
    GL.Program
    (a -> IO ())
    Int

data Program = Program GL.Program (IO ()) Int

version :: String
version = "#version 330 core\n"

compile :: forall i m o. (GPU i, HasType m, HasType o) =>
    Expr (i -> (Vec4 Float, m)) -> Expr (m -> o) -> [i] -> IO Program
compile vert frag xs =
    let inPat   = pat "prim_input_var"     :: Pat i -- TODO FIX?
        mPat    = pat "prim_vert2frag_var" :: Pat m -- TODO FIX?
        outPat  = pat "prim_output_var"    :: Pat o -- TODO FIX?

        inDefV  = defineTop In inPat
        outDefV = defineTop Out mPat
        apExprV = vert :$ Var (V "prim_input_var" $ typeOf (undefined :: i))
        (exprVA, exprVB) = unPair apExprV
        vertStm = (mPat =: exprVB) >> (pat "gl_Position" =: exprVA)
        mainV   = Definition Nothing "main" vertStm
        vStr    = version ++ inDefV ++ outDefV ++ toGLSL mainV

        inDefF  = defineTop In mPat
        outDefF = defineTop Out outPat
        apExprF = frag :$ Var (V "prim_vert2frag_var" $ typeOf (undefined :: m))
        mainF   = Definition Nothing "main" (outPat =: apExprF)
        fStr    = version ++ inDefF ++ outDefF ++ toGLSL mainF
    in do
        prog <- compileAndLink vStr fStr

        attr <- toAttr xs inPat prog

        return $ Program prog (bindAttr attr) $ length xs

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

defineTop :: Qualifier -> Pat a -> String
defineTop q (l :* r) = defineTop q l ++ defineTop q r
defineTop q (BaseG (V n t)) = toGLSL q++" "++toGLSL t++" "++n++";\n"
defineTop _ UnitG = ""

-- Attr

data Attr a = Attr
    GL.BufferObject
    GL.AttribLocation
    (GL.VertexArrayDescriptor a)

bindAttr :: Attr a -> IO ()
bindAttr (Attr buffer location descriptor) = do
    GL.vertexAttribArray location GL.$= GL.Enabled
    GL.bindBuffer GL.ArrayBuffer GL.$= Just buffer
    GL.vertexAttribPointer location GL.$= (GL.ToFloat, descriptor)

------------------
-- Buffer utils --
------------------

makeBuffer :: forall a. (Storable a) =>
    GL.BufferTarget -> [a] -> IO GL.BufferObject
makeBuffer target elems = do
    arrayBuffer <- GL.genObjectName

    GL.bindBuffer target GL.$= Just arrayBuffer

    withArrayLen elems $ \len ptr ->
        let n = fromIntegral $ len * sizeOf (error "makeBuffer" :: a)
        in GL.bufferData target GL.$= (n, ptr, GL.StaticDraw)

    return arrayBuffer

replaceBuffer :: forall a. Storable a =>
    GL.BufferTarget -> [a] -> Int -> IO ()
replaceBuffer target elems len =
    withArray elems $ \ptr -> do
        let dataSize = fromIntegral $ len * sizeOf (error "replaceBuffer" :: a)
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
resizeScene _ width height = do
    -- Make viewport the same size as the window.
    GL.viewport GL.$= (GL.Position 0 0,
                    GL.Size (fromIntegral width) (fromIntegral height))



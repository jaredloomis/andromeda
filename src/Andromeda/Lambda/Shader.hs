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
module Andromeda.Lambda.Shader where

import GHC.Stack (errorWithStackTrace)
import Foreign.Storable
import Foreign.Marshal.Array (withArrayLen, withArray)
import Foreign.Ptr (nullPtr)
import Data.String (fromString)
import Data.Word (Word, Word8)
import Control.Applicative (Applicative)
import Control.Monad.State

import Data.Vec ((:.)(..), Vec4, Vec3, Vec2)

import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.UI.GLFW as GLFW

import Andromeda.Lambda.Expr
import Andromeda.Lambda.Type
import Andromeda.Lambda.Glom
import Andromeda.Lambda.GLSL

-- | A State monad that tracks whether the
--   state has been changed. This can be used
--   to prevent unnecessary buffer rewrites.
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
-- Pair
instance (GPU a, GPU b) => GPU (a, b) where
    toAttr xs (lp `PairG` rp) prog = do
        la <- toAttr (map fst xs) lp prog
        ra <- toAttr (map snd xs) rp prog
        return $ la `PairG` ra
    toAttr _ _ _ = errorWithStackTrace $
        "toAttr (a, b): pair was constructed in some " ++
        "unknown way."

toAttrPrim :: (Storable a, Storable b, HasType b) =>
    [a] -> GL.DataType -> String -> GL.Program -> IO (Attr b)
toAttrPrim xs glType name prog =
    let len = fromIntegral $ length xs
        descriptor = GL.VertexArrayDescriptor len glType 0 nullPtr
    in do
        buffer <- makeBuffer GL.ArrayBuffer xs
        location <- GL.get $ GL.attribLocation prog name
        return $ BaseG (AttrPrim buffer location descriptor)

-- Running a Program --

data ShaderGalaxy =
    PureGalaxy

mainLoopSF :: GLFW.Window -> Program g ->
              g -> (g -> g) -> IO ()
mainLoopSF win prog g update = do
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]
    drawSF prog g

    endFrame
    shouldClose <- GLFW.windowShouldClose win
    unless shouldClose $
        mainLoopSF win prog (update g) update
  where
    endFrame = do
        GLFW.swapBuffers win
        GLFW.pollEvents

drawSF :: Program g -> g -> IO ()
drawSF (Program prog update bindInVars len) g = do
    GL.currentProgram GL.$= Just prog
    update g
    bindInVars
    GL.drawArrays GL.Triangles 0 $ fromIntegral len
    GL.bindBuffer GL.ArrayBuffer GL.$= Nothing
    GL.currentProgram GL.$= Nothing

-- Compiling to Program --

data Program g = Program
    GL.Program
    (g -> IO ())
    (IO ())
    Int

version :: String
version = "#version 330 core\n"

compile :: forall i m o g. (GPU i, HasType m, HasType o) =>
    Expr (i -> (Vec4 Float, m)) -> Expr (m -> o) ->
    [i] -> (g -> [i]) -> IO (Program g)
compile vert frag xsi xsf =
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

        attr <- toAttr xsi inPat prog

        return $ Program prog (updateAttr attr xsf) (bindAttr attr) $ length xsi

updateAttr :: Attr a -> (g -> [a]) -> g -> IO ()
updateAttr (BaseG (AttrPrim buffer _ (GL.VertexArrayDescriptor len _ _ _))) xsf g =
    let xs = xsf g
    in do
        GL.bindBuffer GL.ArrayBuffer GL.$= Just buffer
        replaceBuffer GL.ArrayBuffer xs $ fromIntegral len
updateAttr (a `PairG` b) xsf g = do
    updateAttr a (map fst . xsf) g
    updateAttr b (map snd . xsf) g
updateAttr UnitG _ _ = return ()

defineTop :: Qualifier -> Pat a -> String
defineTop q (l `PairG` r) = defineTop q l ++ defineTop q r
defineTop q (BaseG (V n t)) = toGLSL q++" "++toGLSL t++" "++n++";\n"
defineTop _ UnitG = ""

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

-- Attr

type Attr a = Glom AttrPrim a

data AttrPrim a where
    AttrPrim :: Storable a =>
                GL.BufferObject ->
                GL.AttribLocation ->
                GL.VertexArrayDescriptor a ->
                AttrPrim a

bindAttr :: Attr a -> IO ()
bindAttr (BaseG (AttrPrim buffer location descriptor)) = do
    GL.vertexAttribArray location GL.$= GL.Enabled
    GL.bindBuffer GL.ArrayBuffer GL.$= Just buffer
    GL.vertexAttribPointer location GL.$= (GL.ToFloat, descriptor)
bindAttr (a `PairG` b) = do
    bindAttr a
    bindAttr b
bindAttr UnitG = return ()

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
resizeScene _ width height =
    -- Make viewport the same size as the window.
    GL.viewport GL.$= (GL.Position 0 0,
                    GL.Size (fromIntegral width) (fromIntegral height))

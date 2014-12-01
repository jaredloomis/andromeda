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
        vStr  = compileVert vert
        fStr  = compileFrag frag
    in do
        prog <- compileAndLink vStr fStr
        attr <- toAttr input inPat prog
        return $ Program prog len attr update

{-
updateInput :: Attr a -> Input a -> IO ()
updateInput (BaseG (AttrPrim buffer _ (GL.VertexArrayDescriptor len _ _ _)))
            (InnInput xs) = do
        GL.bindBuffer GL.ArrayBuffer GL.$= Just buffer
        replaceBuffer GL.ArrayBuffer xs $ fromIntegral len
updateInput (PairG la ra) (PairI li ri) = do
    updateInput la li
    updateInput ra ri
updateInput UnitG _ = error "updateInput: UnitG."
updateInput _ _     = error "updateInput: unmatched case."

inputToAttr :: GL.Program -> Pat a -> Input a -> IO (Attr a)
inputToAttr prog (PairG lp rp) (PairI li ri) = do
    la <- inputToAttr prog lp li
    ra <- inputToAttr prog rp ri
    return $ la `PairG` ra
inputToAttr prog patt (InnInput xs) =
    toAttr xs patt prog
inputToAttr prog (UnifG patt) (UniformInput x) =
--    _ x patt prog
    toUniform (Unif x) patt prog
inputToAttr _ _ _ = errorWithStackTrace "inputToAttr: impossible (?) case!"
-}

{-
-- | A State monad that tracks whether the
--   state has been changed. This can be used
--   to prevent unnecessary buffer rewrites.
newtype Change s a = Change {
    runChange :: State (s, Bool) a
    } deriving (Functor, Applicative, Monad)

instance MonadState s (Change s) where
    get = Change (fst `fmap` get)
    put s = Change $ put (s, True)

-----------------------
-- HasAttr typeclass --
-----------------------

class HasType a => HasAttr a where
    toAttr :: [a] -> Pat a -> GL.Program -> IO (Attr a)

    toUniform :: a -> Pat a -> GL.Program -> IO (Attr (Unif a))
    bindUniform :: Attr (Unif a) -> IO ()

-- Scalars
instance HasAttr Float where
    toAttr xs (BaseG (V name _)) =
        toAttrPrim xs GL.Float name
    toUniform = toUnifPrim
    bindUniform (BaseG (UniformPrim x location)) =
        GL.uniform location GL.$= GL.Index1 (unsafeCoerce x :: GL.GLfloat)
    bindUniform _ = errorWithStackTrace
        "bindUniform Float: Given an AttrPrim."
instance HasAttr Int where
    toAttr xs (BaseG (V name _)) =
        toAttrPrim xs GL.Int name
    toUniform = toUnifPrim
    bindUniform (BaseG (UniformPrim x location)) =
        GL.uniform location GL.$= GL.Index1 (unsafeCoerce x :: GL.GLint)
    bindUniform _ = errorWithStackTrace
        "bindUniform Int: Given an AttrPrim."
instance HasAttr Word where
    toAttr xs (BaseG (V name _)) =
        toAttrPrim xs GL.UnsignedInt name
    toUniform = toUnifPrim
    bindUniform (BaseG (UniformPrim x location)) =
        GL.uniform location GL.$= GL.Index1 (fromIntegral x :: GL.GLuint)
    bindUniform _ = errorWithStackTrace
        "bindUniform Word: Given an AttrPrim."
instance HasAttr Bool where
    toAttr xs (BaseG (V name _)) =
        toAttrPrim (map boolToWord8 xs) GL.UnsignedByte name
      where
        boolToWord8 :: Bool -> Word8
        boolToWord8 True  = 1
        boolToWord8 False = 0
    toUniform = toUnifPrim
    bindUniform (BaseG (UniformPrim x location)) =
        GL.uniform location GL.$= GL.Index1 (boolToUInt x)
      where
        boolToUInt :: Bool -> GL.GLuint
        boolToUInt True  = 1
        boolToUInt False = 0
    bindUniform _ = errorWithStackTrace
        "bindUniform Bool: Given an AttrPrim."
-- Vecs
instance (KnownScalar a, Storable a) => HasAttr (Vec2 a) where
    toAttr xs (BaseG (V name _)) =
        toAttrPrim xs (glScalarType (undefined :: a)) name
    toUniform = toUnifPrim
    bindUniform (BaseG (UniformPrim (x:.y:.()) location)) =
        GL.uniform location GL.$=
            GL.Vertex2 (unsafeCoerce x :: GL.GLfloat)
                       (unsafeCoerce y :: GL.GLfloat)
    bindUniform _ = errorWithStackTrace
        "bindUniform Vec2: Given an AttrPrim."
instance (KnownScalar a, Storable a) => HasAttr (Vec3 a) where
    toAttr xs (BaseG (V name _)) =
        toAttrPrim xs (glScalarType (undefined :: a)) name
    toUniform = toUnifPrim
    bindUniform (BaseG (UniformPrim (x:.y:.z:.()) location)) =
        GL.uniform location GL.$=
            GL.Vertex3 (unsafeCoerce x :: GL.GLfloat)
                       (unsafeCoerce y :: GL.GLfloat)
                       (unsafeCoerce z :: GL.GLfloat)
    bindUniform _ = errorWithStackTrace
        "bindUniform Vec3: Given an AttrPrim."
instance (KnownScalar a, Storable a) => HasAttr (Vec4 a) where
    toAttr xs (BaseG (V name _)) =
        toAttrPrim xs (glScalarType (undefined :: a)) name
    toUniform = toUnifPrim
    bindUniform (BaseG (UniformPrim (x:.y:.z:.w:.()) location)) =
        GL.uniform location GL.$=
            GL.Vertex4 (unsafeCoerce x :: GL.GLfloat)
                       (unsafeCoerce y :: GL.GLfloat)
                       (unsafeCoerce z :: GL.GLfloat)
                       (unsafeCoerce w :: GL.GLfloat)
    bindUniform _ = errorWithStackTrace
        "bindUniform Vec4: Given an AttrPrim."
-- Pair
instance (HasAttr a, HasAttr b) => HasAttr (a, b) where
    toAttr xs (lp `PairG` rp) prog = do
        la <- toAttr (map fst xs) lp prog
        ra <- toAttr (map snd xs) rp prog
        return $ la `PairG` ra
    toAttr _ _ _ = errorWithStackTrace $
        "toAttr (a, b): pair was constructed in some " ++
        "unknown way."

    toUniform x (lp `PairG` rp) prog = do
        la <- toUniform (fst x) lp prog
        ra <- toUniform (snd x) rp prog
        return . UnifG $ la `PairG` ra
    toUniform _ _ _ = errorWithStackTrace $
        "toUniform (a, b): pair was constructed " ++
        "in some unknown way."

    bindUniform (UnifG (lp `PairG` rp)) = do
        bindUniform $ UnifG lp
        bindUniform $ UnifG rp
{-
        errorWithStackTrace $
            "bindUniform given:\n  'UnifG (PairG _ _)'\n" ++
            "should be:\n  'PairG (UnifG _) (UnifG _)'"
-}
    bindUniform _ = errorWithStackTrace $
        "bindUniform (a, b): pair was constructed " ++
        "in some unknown way."
-- Uniforms
{-
instance HasAttr a => HasAttr (Unif a) where
    toAttr _ _ = errorWithStackTrace
        "toAttr called on a Unif."
    toUniform (Unif x) (UnifG patt) prog =
        _ $ toUniform x patt prog
-}


toAttrPrim :: (Storable a, Storable b, HasType b) =>
    [a] -> GL.DataType -> String -> GL.Program -> IO (Attr b)
toAttrPrim xs glType name prog =
    let len = fromIntegral $ length xs
        descriptor = GL.VertexArrayDescriptor len glType 0 nullPtr
    in do
        buffer <- makeBuffer GL.ArrayBuffer xs
        location <- GL.get $ GL.attribLocation prog name
        return $ BaseG (AttrPrim buffer location descriptor)

toUnifPrim :: HasAttr a => a -> Pat a -> GL.Program -> IO (Attr (Unif a))
toUnifPrim x (BaseG (V name _)) prog = do
    location <- GL.get $ GL.uniformLocation prog name
    return $ BaseG (UniformPrim x location)
toUnifPrim _ _ _ = errorWithStackTrace
    "toUnifPrim: Given non-prim pat."


-- Running a Program --

data ShaderGalaxy g =
    PureGalaxy (g ->    g) (Program g)
  | IOGalaxy   (g -> IO g) (Program g)

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
-}

-- Compiling to Program --
{-
data Program g = Program
    GL.Program
    (g -> IO ())
    (IO ())
    Int
version :: String
version = "#version 330 core\n"

data Input a where
    UniformInput :: HasAttr a =>  a  -> Input (Unif a)
    InnInput     :: HasAttr a => [a] -> Input a
    PairI        :: Input a -> Input b -> Input (a, b)

inputLen :: Input a -> Int
inputLen (InnInput xs) = length xs
inputLen (UniformInput _) = 1
inputLen (PairI a b) = max (inputLen a) (inputLen b)

compile :: forall i m o g. (HasType i, HasType o, HasType m, HasGLSL m) =>
    Expr (i -> (Vec4 Float, m)) -> Expr (m -> o) ->
    Input i -> (g -> Input i) ->
    IO (Program g)
compile vert frag input update =
    let len = inputLen input
        inPat = pat "prim_input_var" :: Pat i
        vStr  = compileVert vert
        fStr  = compileFrag frag
    in do
        prog <- compileAndLink vStr fStr
        attr <- inputToAttr prog inPat input
        return $ Program prog (updateInput attr . update) (bindAttr attr) len

updateInput :: Attr a -> Input a -> IO ()
updateInput (BaseG (AttrPrim buffer _ (GL.VertexArrayDescriptor len _ _ _)))
            (InnInput xs) = do
        GL.bindBuffer GL.ArrayBuffer GL.$= Just buffer
        replaceBuffer GL.ArrayBuffer xs $ fromIntegral len
updateInput (PairG la ra) (PairI li ri) = do
    updateInput la li
    updateInput ra ri
updateInput UnitG _ = error "updateInput: UnitG."
updateInput _ _     = error "updateInput: unmatched case."

inputToAttr :: GL.Program -> Pat a -> Input a -> IO (Attr a)
inputToAttr prog (PairG lp rp) (PairI li ri) = do
    la <- inputToAttr prog lp li
    ra <- inputToAttr prog rp ri
    return $ la `PairG` ra
inputToAttr prog patt (InnInput xs) =
    toAttr xs patt prog
inputToAttr prog (UnifG patt) (UniformInput x) =
--    _ x patt prog
    toUniform (Unif x) patt prog
inputToAttr _ _ _ = errorWithStackTrace "inputToAttr: impossible (?) case!"
-}

compileVert :: forall i o. (HasType i, HasType o, HasGLSL o) =>
    Expr (i -> (Vec4 Float, o)) -> String
compileVert expr =
    let inPat   =  pat "prim_input_var"     :: Pat i
        outPat  =  pat "prim_vert2frag_var" :: Pat o
        inDefV  = defineTop In inPat
        outDefV = defineTop Out outPat
        apExprV = expr :$ Var (V "prim_input_var" $ typeOf (undefined :: i))
        (exprVA, exprVB) = unPair apExprV
        vertStm = (outPat =: exprVB) >> (pat "gl_Position" =: exprVA)
        mainV   = Definition Nothing "main" vertStm
        vStr    = version ++ inDefV ++ outDefV ++ toGLSL mainV
    in vStr

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
    in fStr

{-
compile :: forall i m o g. (HasAttr i, HasType m, HasType o) =>
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

        return $ Program prog (updateAttr attr xsf)
                              (bindAttr attr)
                              (length xsi)
-}
{-
updateAttr :: Attr a -> (g -> [a]) -> g -> IO ()
updateAttr (BaseG (AttrPrim buffer _ (GL.VertexArrayDescriptor len _ _ _)))
           xsf g =
    let xs = xsf g
    in do
        GL.bindBuffer GL.ArrayBuffer GL.$= Just buffer
        replaceBuffer GL.ArrayBuffer xs $ fromIntegral len
updateAttr (a `PairG` b) xsf g = do
    updateAttr a (map fst . xsf) g
    updateAttr b (map snd . xsf) g
updateAttr UnitG _ _ = return ()
-}

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

----------
-- Attr --
----------

type Attr a = Glom AttrPrim a

data AttrPrim a where
    AttrPrim :: Storable a =>
                GL.BufferObject ->
                GL.AttribLocation ->
                GL.VertexArrayDescriptor a ->
                AttrPrim a
    UniformPrim :: HasAttr a =>
        a -> GL.UniformLocation -> AttrPrim a

{-
bindAttr :: Attr a -> IO ()
bindAttr (BaseG (AttrPrim buffer location descriptor)) = do
    GL.vertexAttribArray location GL.$= GL.Enabled
    GL.bindBuffer GL.ArrayBuffer GL.$= Just buffer
    GL.vertexAttribPointer location GL.$= (GL.ToFloat, descriptor)
bindAttr unif@(BaseG UniformPrim{}) = bindUniform unif
bindAttr (a `PairG` b) = do
    bindAttr a
    bindAttr b
bindAttr UnitG = return ()
-}

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

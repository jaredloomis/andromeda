{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module Andromeda.Lambda.Shader where

import GHC.Stack (errorWithStackTrace)
import Foreign.Storable
import Foreign.Marshal.Array (withArrayLen, withArray)
import Foreign.Ptr (nullPtr)
import Data.String (fromString)
import Control.Monad.State
import Debug.Trace (trace)
import Unsafe.Coerce (unsafeCoerce)
import System.Exit (exitFailure)
import Data.Functor.Identity (runIdentity)

import Data.Vec ((:.)(..), Vec4, Vec3)

import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.UI.GLFW as GLFW

import Andromeda.Lambda.Expr
import Andromeda.Lambda.Type
import Andromeda.Lambda.Glom
import Andromeda.Lambda.GLSL
import Andromeda.Lambda.HasAttr

-- Idea
--data Statement' where
--    Assign :: Pat a -> Expr (i -> a) -> Input i -> Statement'

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

inputPat  :: String
inputPat  = "andromeda_input"
interPat  :: String
interPat  = "andromeda_inter"
outputPat :: String
outputPat = "andromeda_output"

version :: String
version = "#version 330 core\n"

inputLen :: Input a -> Int
inputLen (InInput xs) = length xs
inputLen (UniformInput _) = 1
inputLen (LamI a b) = max (inputLen a) (inputLen b)
inputLen (PairI a b) = max (inputLen a) (inputLen b)

type family Last a where
    Last (a -> b) = Last b
    Last  a       = a
type family Init a where
    Init (a -> b) = a

{-
comp :: forall i m o g last init.
    (HasAttr i, HasType o, HasType m, HasGLSL m,
     Last i ~ last, Init i ~ init) =>
    Expr (i -> (Vec4 Float, m)) -> Expr (m -> o) ->
    Input i -> (g -> Input i) ->
    IO (Program g i)
comp vert frag input update = undefined
-}

compile :: forall i m o g. (HasAttr i, HasType o, HasType m, HasGLSL m) =>
    Expr (i -> (Vec4 Float, m)) -> Expr (m -> o) ->
    Input i -> (g -> Input i) ->
    IO (Program g i)
compile vert frag input update =
    let len = inputLen input
        inPat = pat inputPat :: Pat i
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
qualifiers (LamI l r) = PairQ (qualifiers l) (qualifiers r)
qualifiers (PairI l r) = PairQ (qualifiers l) (qualifiers r)

compileVert :: forall i o. (HasType i, HasType o, HasGLSL o) =>
    Expr (i -> (Vec4 Float, o)) -> Quals -> String
compileVert expr quals =
    let inPat   = pat inputPat :: Pat i
        outPat  = pat interPat :: Pat o
        inDefV  = defineTopQ quals inPat
        outDefV = defineTop Out outPat
        apExprV = expr :$ Var (V inputPat $ typeOf (undefined :: i))
        (exprVA, exprVB) = unPair apExprV
        vertStm = (outPat =: exprVB) >> (pat "gl_Position" =: exprVA)
        mainV   = Definition Nothing "main" vertStm
        vStr    = version ++ inDefV ++ outDefV ++ toGLSL mainV
    in trace vStr vStr

compileFrag :: forall i o. (HasType i, HasType o) =>
    Expr (i -> o) -> String
compileFrag expr =
    let inPat   =  pat interPat :: Pat i
        outPat  =  pat outputPat :: Pat o
        inDefF  = defineTop In inPat
        outDefF = defineTop Out outPat
        apExprF = expr :$ Var (V interPat $ typeOf (undefined :: i))
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

---------

--compile' :: Expr a -> Expr b -> IO (Program)
--compile' vert frag =

compileToGLSL :: Pat a -> Expr a -> String
compileToGLSL outPat expr =
    let (expr', input) = runIdentity $ runStateT (collectInput expr) []
        declIn = concatMap declareInput input
    in "#version 330 core\n" ++ declIn ++
       "void main() {\n" ++ mainAction outPat expr' ++ "\n}"

mainAction :: Pat a -> Expr a -> String
mainAction UnitG _ = ""
mainAction (BaseG (V name _)) expr =
    name ++ " = " ++ toGLSL expr ++ ";\n"
mainAction (l `PairG` r) expr =
    let (a, b) = unPair expr
    in mainAction l a ++ mainAction r b

declareInput :: Input' -> String
declareInput (InIn   _ ty n) =
    "in " ++ toGLSL ty ++ " " ++ n ++ ";\n"
declareInput (InUnif _ ty n) =
    "uniform " ++ toGLSL ty ++ " " ++ n ++ ";\n"

collectInput :: Expr a -> State [Input'] (Expr a)
collectInput (Lit (LitIn xs ty n)) = do
    tellInput $ InIn xs ty n
    return $ Var (V n ty)
collectInput (Lit (LitUnif x ty n)) = do
    tellInput $ InUnif x ty n
    return $ Var (V n ty)
collectInput (f :$ x) =
    (:$) <$> collectInput f <*> collectInput x
collectInput expr = return expr

tellInput :: Input' -> State [Input'] ()
tellInput i@(InIn _ _ n) = do
    xs <- get
    unless (any isDuplicate xs) $
        modify (++[i])
  where
    isDuplicate (InIn   _ _ n') = n == n'
    isDuplicate (InUnif _ _ n') = n == n'
tellInput i@(InUnif _ _ n) = do
    xs <- get
    unless (any isDuplicate xs) $
        modify (++[i])
  where
    isDuplicate (InIn   _ _ n') = n == n'
    isDuplicate (InUnif _ _ n') = n == n'

----------------------------------------------------
-- Functions to compile shaders to a 'GL.Program' --
----------------------------------------------------

compileAndLink :: String -> String -> IO GL.Program
compileAndLink vert frag = do
    v <- compileGLSL vert GL.VertexShader
    f <- compileGLSL frag GL.FragmentShader

    program <- GL.createProgram

    GL.attachShader program v
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
        putStrLn =<< GL.get (GL.shaderInfoLog shader)

    return shader

----------------
-- GLFW Stuff --
----------------

openWindow :: IO GLFW.Window
openWindow = do
    glfwOk <- GLFW.init

    unless glfwOk $ do
        putStrLn "GLFW initialize failed!!"
        exitFailure

    -- Give GLFW some hints.
    mapM_ GLFW.windowHint
        [GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Compat,
         GLFW.WindowHint'DepthBits 16,
         GLFW.WindowHint'Samples 4,
         GLFW.WindowHint'Resizable True,
         GLFW.WindowHint'ClientAPI GLFW.ClientAPI'OpenGL,
         GLFW.WindowHint'ContextVersionMajor 3,
         GLFW.WindowHint'ContextVersionMinor 3]

    -- Open window.
    mwin <- GLFW.createWindow
                    800
                    600
                    "GLFW Window" Nothing Nothing

    win <- maybe (putStrLn "GLFW create window failed!!" >>
                  exitFailure >> return undefined)
                 return
                 mwin

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

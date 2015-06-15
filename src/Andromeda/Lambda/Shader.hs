{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Andromeda.Lambda.Shader where

import GHC.Stack (errorWithStackTrace)
import Data.String (fromString)
import Control.Monad.State
import System.Exit (exitFailure)
import Data.Functor.Identity (runIdentity)
import Data.Monoid ((<>))

import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.UI.GLFW as GLFW

import Andromeda.Lambda.Expr
import Andromeda.Lambda.Type
import Andromeda.Lambda.Glom
import Andromeda.Lambda.VertexBuffer

---------------------
-- Running program --
---------------------

{-
mainLoop :: HasVertex i =>
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

drawProgram :: HasVertex i => Program g i -> g -> IO (Program g i)
drawProgram (Program prog len attr update) g = do
    GL.currentProgram GL.$= Just prog

    let input = update g
    attr' <- replaceAttr input attr
    bindVertex attr'

    GL.drawArrays GL.Triangles 0 $ fromIntegral len
    GL.bindBuffer GL.ArrayBuffer GL.$= Nothing
    GL.currentProgram GL.$= Nothing

    return $ Program prog len attr update
-}

mainLoop :: GLFW.Window -> Program -> IO ()
mainLoop win prog = do
    prog' <- drawProgram prog
    nextFrame win $ mainLoop win prog'

nextFrame :: GLFW.Window -> IO () -> IO ()
nextFrame win action = do
    endFrame
    shouldClose <- GLFW.windowShouldClose win
    unless shouldClose action
  where
    endFrame = do
        GLFW.swapBuffers win
        GLFW.pollEvents

drawProgram :: Program -> IO Program
drawProgram prog = do
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]
    drawProgram' prog

drawProgram' :: Program -> IO Program
drawProgram' (Program prog len attrs) = do
    GL.currentProgram GL.$= Just prog
    mapM_ bindVertex' attrs

    GL.drawArrays GL.Triangles 0 $ fromIntegral len
    GL.bindBuffer GL.ArrayBuffer GL.$= Nothing
    GL.currentProgram GL.$= Nothing

    return $ Program prog len attrs

------------------------
-- Compile to program --
------------------------

data Program = Program GL.Program Int [VertexBuffer]

data Statement where
    AssignS :: HasType a => String -> Expr a -> Statement
    OutS    :: HasType a => String -> Expr a -> Statement
    ThenS   :: Statement -> Statement -> Statement

data ShaderSource = ShaderSource {
    shaderSrcTopLevelDecls :: String,
    shaderSrcMain          :: String
    } deriving (Show, Eq)

instance Monoid ShaderSource where
    mempty = ShaderSource "" ""
    mappend (ShaderSource ad am) (ShaderSource bd bm) =
        ShaderSource (ad ++ bd) (am ++ bm)

instance HasGLSL ShaderSource where
    toGLSL (ShaderSource declSrc mainSrc) =
        declSrc ++ "\nvoid main() {\n" ++ mainSrc ++ "\n}"

compile :: Statement -> Statement -> IO Program
compile vert frag =
    let (vSrc, vi) = compileStatement vert
        vStr'      = "#version 330 core\n" ++ toGLSL vSrc
        (fSrc, fi) = compileStatement frag
        fStr'      = "#version 330 core\n" ++ toGLSL fSrc
        input      = vi ++ fi
    in do
        prog <- compileAndLink vStr' fStr'
        attr <- mapM (`toVertex'` prog) input
        return $ Program prog (inLen input) attr
  where
    inLen (InIn xs _ _ :  _) = length xs
    inLen (_           : xs) = inLen xs
    inLen []                 = 0

compileStatement :: Statement -> (ShaderSource, [Input'])
compileStatement (AssignS name expr)  = compileToGLSL (pat name) expr
compileStatement (OutS name expr) =
    let ty   = typeOfE expr
        decl = "out " ++ toGLSL ty ++ " " ++ name ++ ";\n"
        (ShaderSource decl' mainSrc, input) = compileToGLSL (pat name) expr
    in (ShaderSource (decl <> decl') mainSrc, input)
compileStatement (ThenS a b) =
    let (as, ai) = compileStatement a
        (bs, bi) = compileStatement b
    in (as <> bs, ai ++ bi)

compileToGLSL :: Pat a -> Expr a -> (ShaderSource, [Input'])
compileToGLSL outPat expr =
    let (expr', input) = runIdentity $ runStateT (collectInput expr) []
        declIn = concatMap declareInput input
    in (ShaderSource declIn $ mainAction outPat expr', input)

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
collectInput (Lit (LitIn n ty xs)) = do
    tellInput $ InIn xs ty n
    return $ Var (V n ty)
collectInput (Lit (LitUnif n ty x)) = do
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
-- Functions to compile shaders to a 'GL.Program --
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

------------
-- unPair --
------------

unPair :: forall a b. (HasType a, HasType b) =>
    Expr (a, b) -> (Expr a, Expr b)
unPair = unPair' . betaReduce

unPair' :: forall a b. (HasType a, HasType b) =>
    Expr (a, b) -> (Expr a, Expr b)
unPair' (Lit Pair :$ a :$ b) = (a, b)
unPair' (Var (V name _)) =
    case pat name :: Pat (a, b) of
        PairG (BaseG varA) (BaseG varB) ->
            (Var varA, Var varB)
        _ -> errorWithStackTrace
            "unPair': 'pat' returned an unexpected format."
unPair' expr = errorWithStackTrace $
    "unPair: Pair was made through some " ++
    "unknown operation:\n" ++ show expr

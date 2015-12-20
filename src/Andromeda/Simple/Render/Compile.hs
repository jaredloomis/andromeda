{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
-- This needs a complete rewrite
module Andromeda.Simple.Render.Compile where

import Control.Monad.State
import Data.Monoid ((<>))
import Data.List (isInfixOf)
import Data.String (fromString)
import Data.Foldable (foldrM)
import System.Exit (exitFailure)
import Control.Monad.Free (Free(..), liftF)

import qualified Data.Map as M

import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.UI.GLFW as GLFW

import Andromeda.Simple.Expr
import Andromeda.Simple.Type
import Andromeda.Simple.GLSL
import Andromeda.Simple.Var

import Andromeda.Simple.Render.Mesh
import Andromeda.Simple.Render.VertexBuffer

type Statement a = Free StatementF a

data StatementF next where
    AssignF :: Typed a => String -> Expr a -> next -> StatementF next
    OutF    :: Typed a => String -> Expr a -> next -> StatementF next

instance Functor StatementF where
    fmap f (AssignF n e next) = AssignF n e $ f next
    fmap f (OutF    n e next) = OutF    n e $ f next

(=:) :: Typed a => String -> Expr a -> Statement ()
(=:) name expr = liftF $ AssignF name expr ()
infixr 2 =:

out :: Typed a => String -> Expr a -> Statement ()
out name expr = liftF $ OutF name expr ()

data Renderer = Renderer {
    glProgram       :: !GL.Program,

    attribLocation  :: M.Map String GL.AttribLocation,
    uniformLocation :: M.Map String GL.UniformLocation,

    rendererMeshes  :: [CompiledMesh],
    renderLen       :: !GL.GLint
    }

----------------------
-- Running Renderer --
----------------------

mainLoop :: GLFW.Window -> Renderer -> IO ()
mainLoop win renderer = do
    renderer' <- drawRenderer renderer
    nextFrame win $ mainLoop win renderer'

nextFrame :: GLFW.Window -> IO () -> IO ()
nextFrame win action = do
    endFrame
    shouldClose <- GLFW.windowShouldClose win
    unless shouldClose action
  where
    endFrame = do
        GLFW.swapBuffers win
        GLFW.pollEvents

drawRenderer :: Renderer -> IO Renderer
drawRenderer renderer@(Renderer prog _ _ _ len) = do
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]

    GL.currentProgram GL.$= Just prog
    renderer' <- bindMeshes renderer

    GL.drawArrays GL.Triangles 0 len

    GL.bindBuffer GL.ArrayBuffer GL.$= Nothing
    GL.currentProgram GL.$= Nothing

    return renderer'

updateRenderLen :: Renderer -> Renderer
updateRenderLen renderer@(Renderer _ _ _ xs _) =
    renderer {
        renderLen = maximum $
            map (\(CompiledMesh ats _ _) -> maximum $
            map (bufferLen . snd) ats) xs
        }

bindMeshes :: Renderer -> IO Renderer
bindMeshes renderer@(Renderer prog _ _ meshes _) =
    foldrM bindMesh renderer meshes
  where
    bindMesh :: CompiledMesh -> Renderer -> IO Renderer
    bindMesh (CompiledMesh attrs unifs _) render'@(Renderer _ attrMap unifMap _ _) = do
        attrMap' <- foldrM bindAttr attrMap attrs
        unifMap' <- foldrM bindUnif unifMap unifs
        return $ render' {
                attribLocation  = attrMap',
                uniformLocation = unifMap'
                }

    bindUnif :: (String, Uniform) ->
                M.Map String GL.UniformLocation ->
                IO (M.Map String GL.UniformLocation)
    bindUnif (attr, Uniform unif) unifMap = do
        (changed, loc) <- maybe ((True,) <$> GL.get (GL.uniformLocation prog attr))
                                (return . (False,))
                                (M.lookup attr unifMap)
        GL.uniform loc GL.$= unif
        return $ if changed then M.insert attr loc unifMap else unifMap

    bindAttr :: (String, VertexBuffer) ->
                M.Map String GL.AttribLocation ->
                IO (M.Map String GL.AttribLocation)
    bindAttr (attr, buf) attrMap = do
        (changed, loc) <- maybe ((True,) <$> GL.get (GL.attribLocation prog attr))
                                (return . (False,))
                                (M.lookup attr attrMap)
        bindBuffer buf loc
        return $ if changed then M.insert attr loc attrMap else attrMap

------------------------
-- Modifying Renderer --
------------------------

addMesh :: Mesh -> Renderer -> IO Renderer
addMesh mesh renderer = do
    mesh' <- compileMesh mesh
    return . updateRenderLen $ renderer {
        rendererMeshes = mesh' : rendererMeshes renderer
        }

---------------
-- Compiling --
---------------

data ShaderSource = ShaderSource {
    shaderSrcTopLevelDecls :: String,
    shaderSrcMain          :: String
    } deriving (Show, Eq)

instance Monoid ShaderSource where
    mempty = ShaderSource "" ""
    mappend (ShaderSource ad am) (ShaderSource bd bm) =
        ShaderSource (ad ++ bd) (am ++ bm)

instance GLSL ShaderSource where
    toGLSL (ShaderSource declSrc mainSrc) =
        declSrc ++ "\nvoid main() {\n" ++ mainSrc ++ "\n}"

compile :: Statement () -> Statement () -> IO Renderer
compile vert frag =
    let vStr = version ++ toGLSL (compileStatement vert)
        fStr = version ++ toGLSL (compileStatement frag)
    in do
        prog  <- compileAndLink vStr fStr
        return $ Renderer prog M.empty M.empty [] 0
  where
    version :: String
    version = "#version 330 core\n"

{-
compile :: Statement -> Statement -> IO Program
compile vert frag =
    let vSrc   = compileStatement' vert
        vStr'  = "#version 330 core\n" ++ toGLSL vSrc
        fSrc   = compileStatement' frag
        fStr'  = "#version 330 core\n" ++ toGLSL fSrc
        input  = vi ++ fi
    in do
        prog <- compileAndLink vStr' fStr'
        attr <- mapM (`toVertex'` prog) input
        return $ Program prog (inLen input) attr
  where
    inLen (InIn xs _ _ :  _) = length xs
    inLen (_           : xs) = inLen xs
    inLen []                 = 0
-}

compileStatement :: Statement () -> ShaderSource
compileStatement =
    flip evalState [] . fmap (foldr (<>) mempty) .
    mapM compileStatement' . statementToList

compileStatement' :: StatementF () -> State [String] ShaderSource
compileStatement' (AssignF name expr ()) = compileToGLSL name expr
compileStatement' (OutF name expr ()) = do
    let ty   = typeOfE expr
        decl = "out " ++ toGLSL ty ++ " " ++ name ++ ";\n"
    ShaderSource decl' mainSrc <- compileToGLSL name expr
    return $ ShaderSource (decl <> decl') mainSrc
{-
compileStatement' (ThenS a b) =
    (<>) <$> compileStatement' a <*> compileStatement' b
-}

statementToList :: Statement () -> [StatementF ()]
statementToList (Free (AssignF name expr next)) =
    AssignF name expr () : statementToList next
statementToList (Free (OutF name expr next)) =
    OutF name expr () : statementToList next
statementToList (Pure ()) = []

compileToGLSL :: String -> Expr a -> State [String] ShaderSource
compileToGLSL outName expr = do
    declIn <- declareInput expr
    return . ShaderSource declIn $ mainAction outName expr

mainAction :: String -> Expr a -> String
mainAction name (Lit Pair :$ a :$ b) =
    let (l, r) = pairPat name
    in mainAction l a ++ mainAction r b
mainAction name (Var (V n (tyA :*: tyB))) =
    let (l, r) = pairPat name
        (varA, varB) = pairPat n
    in mainAction l (Var (V varA tyA)) ++ mainAction r (Var (V varB tyB))
mainAction name expr = name ++ " = " ++ toGLSL expr ++ ";\n"

declareInput :: Expr a -> State [String] String
declareInput (Lit (Fetch n ty)) = do
    names <- get
    if [n] `isInfixOf` names
        then return ""
        else do
            put (n:names)
            return $ "in " ++ toGLSL ty ++ " " ++ n ++ ";\n"
declareInput (Lit (Unif n ty)) = do
    names <- get
    if [n] `isInfixOf` names
        then return ""
        else do
            put (n:names)
            return $ "uniform " ++ toGLSL ty ++ " " ++ n ++ ";\n"
declareInput (f :$ x) =
    (++) <$> declareInput f <*> declareInput x
declareInput _        = return ""

{-
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
-}

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
--         GLFW.WindowHint'DepthBits 16,
--         GLFW.WindowHint'Samples 4,
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


{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.GLSL.Monad.Primitive where

import Control.Monad (unless)
import Control.Applicative ((<$>))
import Data.Monoid ((<>), mempty)

import Foreign.Ptr (nullPtr)
import Foreign.Marshal.Array (withArray, withArrayLen)
import Foreign.Storable (Storable, sizeOf)

import Data.Vec
    ((:.)(..), Vec3, Mat44, matToLists, perspective,
     multmm, translation, matToList,
     rotationLookAt, rotationY)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL.Raw (glUniformMatrix4fv)
import qualified Graphics.UI.GLFW as GLFW

import Language.GLSL.Monad.GLSL hiding (($=), Proxy, simpleProgram)

---------------------
-- Default actions --
---------------------

openWindow :: IO GLFW.Window
openWindow = do
    _ <- GLFW.init

    -- Give GLFW some hints.
    mapM_ GLFW.windowHint
        [GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core,
         GLFW.WindowHint'Samples 4,
         GLFW.WindowHint'Resizable True,
         GLFW.WindowHint'ClientAPI GLFW.ClientAPI'OpenGL,
         GLFW.WindowHint'ContextVersionMajor 4,
         GLFW.WindowHint'ContextVersionMinor 4]

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
    GL.clearColor $= GL.Color4 0 0 0 1

    -- Enables clearing of the depth buffer
    GL.clearDepth $= 1
    -- Allow depth testing (3D)
    -- Tells OpenGL how to deal with overlapping shapes
    GL.depthFunc $= Just GL.Less

    -- Do not render the backs of faces. Increases performance.
    GL.cullFace $= Just GL.Back

    -- Call resize function.
    (w,h) <- GLFW.getFramebufferSize win
    print (w,h)
    resizeScene win w h 

resizeScene :: GLFW.WindowSizeCallback
-- Prevent divide by 0
resizeScene win w 0 = resizeScene win w 1
resizeScene _ width height =
    -- Make viewport the same size as the window.
    GL.viewport $= (GL.Position 0 0,
                    GL.Size (fromIntegral width) $ fromIntegral height)

cleanup :: ShaderProgram t -> IO ()
cleanup (ShaderProgram program _ _ _) = do
    GL.deleteObjectName program

------------------
-- Buffer utils --
------------------

makeBuffer :: forall a. (Storable a) =>
    GL.BufferTarget -> [a] -> IO GL.BufferObject
makeBuffer target elems = do
    arrayBuffer <- GL.genObjectName

    GL.bindBuffer target $= Just arrayBuffer

    withArrayLen elems $ \len ptr ->
        let n = fromIntegral $ len * sizeOf (error "makeBuffer" :: a)
        in GL.bufferData target $= (n, ptr, GL.StaticDraw)

    return arrayBuffer

replaceBuffer :: forall a. Storable a =>
    GL.BufferTarget -> [a] -> Int -> IO ()
replaceBuffer target elems len =
    withArray elems $ \ptr -> do
        let dataSize = fromIntegral $ len * sizeOf (error "replaceBuffer" :: a)
        GL.bufferData target $= (dataSize, ptr, GL.StaticDraw)

------------------------------------------
-- Different representations of Shaders --
------------------------------------------

data Shader (p :: GL.ShaderType) t =
    Shader (ShaderTypeProxy p) (GLSL t ())

type ShaderSequence t = [WrappedShader t]
data WrappedShader t =
    forall p. (ShaderTypeVal (ShaderTypeProxy p)) =>
        Wrapped (Shader p t)

data ShaderProgram t =
    ShaderProgram
        GL.Program
        (GLSLInfo t)
        [AttribGPU t]
        [UniformGPU t]

data ShaderTypeProxy (t :: GL.ShaderType) = STProxy

class ShaderTypeVal a where
    typeVal :: a -> GL.ShaderType
instance ShaderTypeVal (ShaderTypeProxy GL.VertexShader) where
    typeVal = const GL.VertexShader
instance ShaderTypeVal (ShaderTypeProxy GL.TessControlShader) where
    typeVal = const GL.TessControlShader
instance ShaderTypeVal (ShaderTypeProxy GL.TessEvaluationShader) where
    typeVal = const GL.TessEvaluationShader
instance ShaderTypeVal (ShaderTypeProxy GL.GeometryShader) where
    typeVal = const GL.GeometryShader
instance ShaderTypeVal (ShaderTypeProxy GL.FragmentShader) where
    typeVal = const GL.FragmentShader
instance ShaderTypeVal (ShaderTypeProxy GL.ComputeShader) where
    typeVal = const GL.ComputeShader

-----------------------------------
-- Type family for shader phases --
-----------------------------------

type family NextShaderPhase (s :: GL.ShaderType) :: GL.ShaderType where
    NextShaderPhase GL.VertexShader = GL.TessControlShader
    NextShaderPhase GL.TessControlShader = GL.TessEvaluationShader
    NextShaderPhase GL.TessEvaluationShader = GL.GeometryShader
    NextShaderPhase GL.GeometryShader = GL.FragmentShader
    NextShaderPhase GL.FragmentShader = GL.ComputeShader
    NextShaderPhase GL.ComputeShader = GL.VertexShader

-----------------------
-- Lower level types --
-----------------------

data AttribGPU t =
    forall a. AttribGPU
        GL.BufferObject
        (t -> IO ())
        GL.AttribLocation
        (GL.VertexArrayDescriptor a)
        GL.NumArrayIndices

data UniformGPU t = UniformGPU GL.UniformLocation (t -> IO ())

--------------------------
-- Using ShaderPrograms --
--------------------------

drawProgram :: ShaderProgram t -> t -> IO ()
drawProgram (ShaderProgram prog _ attribs unis) global = do
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]
    GL.currentProgram $= Just prog

    -- Bind all vars.
    mapM_ (bindAttrib global) attribs
    mapM_ (bindUniform global) unis

    -- Draw.
    let len = lenAttr $ head attribs
    GL.drawArrays GL.Triangles 0 len

    -- Unbind ArrayBuffer.
    GL.bindBuffer GL.ArrayBuffer $= Nothing
    -- Turn off shader.
    GL.currentProgram $= Nothing
  where
    lenAttr (AttribGPU _ _ _ _ len) = len

-----------------------------------
-- Shader t p -> ShaderProgram t --
-----------------------------------

createProgram :: ShaderSequence t -> t -> IO (ShaderProgram t)
createProgram shaderSequence global = do
    program <- compileAndLink shaderSequence
    let info@(GLSLInfo ins uniforms _) = gatherInfo shaderSequence

    attrs <- if null ins
                then return []
            else createAttrList program global ins
    unis <- if null uniforms
                then return []
            else createUniformList program uniforms
    return $ ShaderProgram program info attrs unis

compileAndLink :: ShaderSequence t -> IO GL.Program
compileAndLink shaderSeq = do
    shaders <- compileAll shaderSeq
    program <- GL.createProgram
    mapM_ (GL.attachShader program) shaders
    -- mapM_ (\s -> GL.attachShader program s >> GL.deleteObjectName s) shaders
    GL.linkProgram program
    return program

compileAll :: ShaderSequence t -> IO [GL.Shader]
compileAll (Wrapped current : rest) = do
    firstPrograms <- compile current
    otherPrograms <- compileAll rest
    return $ firstPrograms : otherPrograms
compileAll [] = return []

compile :: ShaderTypeVal (ShaderTypeProxy p) =>
            Shader p t -> IO GL.Shader
compile (Shader proxy glsl) =
    let code = generateGLSL glsl
        shaderType = typeVal proxy
    in compileShader shaderType code

compileShader :: GL.ShaderType -> B.ByteString -> IO GL.Shader
compileShader shaderType src = do
    -- Create GL.Shader
    shader <- GL.createShader shaderType
    GL.shaderSourceBS shader $= src
    -- Compile GL.Shader.
    GL.compileShader shader
    -- Check status.
    ok <- GL.get $ GL.compileStatus shader
    unless ok $
        print =<< GL.get (GL.shaderInfoLog shader)

    return shader

gatherInfo :: ShaderSequence t -> GLSLInfo t
gatherInfo (Wrapped (Shader proxy glsl) : shaders) =
    let GLSLInfo ins uniforms outs = evalGLSL glsl
        ins' = case typeVal proxy of
            GL.VertexShader -> ins
            _               -> []
        outs' = case typeVal proxy of
            GL.FragmentShader -> outs
            _                 -> []
    in GLSLInfo ins' uniforms outs' <> gatherInfo shaders
gatherInfo [] = mempty

-- = Dealing with Ins

createAttrList :: GL.Program -> t -> [In t] -> IO [AttribGPU t]
createAttrList progId global ins = do
    mapM (getInGPUInfo progId global) ins

getInGPUInfo :: GL.Program -> t -> In t -> IO (AttribGPU t)
getInGPUInfo prog global i = do
    let name = inName i
        lenValues = inLength i global

    buffer <- inMkBuffer i global

    let updateFunc g = do
            GL.bindBuffer GL.ArrayBuffer $= Just buffer
            inReplaceBuffer i g lenValues

    location <- GL.get . GL.attribLocation prog $ C.unpack name

    let (elemCount, glType) = inDescriptor i
        descriptor = GL.VertexArrayDescriptor elemCount glType 0 nullPtr

    return $ AttribGPU buffer updateFunc location descriptor
                       (fromIntegral $ lenValues)

inMkBuffer :: In t -> t -> IO GL.BufferObject
inMkBuffer (InFloat valFunc _) global = makeBuffer GL.ArrayBuffer $ valFunc global
inMkBuffer (InInt valFunc _) global = makeBuffer GL.ArrayBuffer $ valFunc global
inMkBuffer (InVec2 valFunc _) global = makeBuffer GL.ArrayBuffer $ valFunc global
inMkBuffer (InVec3 valFunc _) global = makeBuffer GL.ArrayBuffer $ valFunc global
inMkBuffer (InVec4 valFunc _) global = makeBuffer GL.ArrayBuffer $ valFunc global
inMkBuffer (InMat4 _ _) _ =
    error "Primitive.inMkBuffer Given Mat4. Idk what to do."
    --map VAOIndex . valFunc
inMkBuffer (InBool _ _) _ =
    error "Primitive.inMkBuffer Given Bool. Idk what to do."
    --map VAOIndex . valFunc

inReplaceBuffer :: In t -> t -> Int -> IO ()
inReplaceBuffer (InFloat valFunc _) global lenVals =
    replaceBuffer GL.ArrayBuffer (valFunc global) lenVals
inReplaceBuffer (InInt valFunc _) global lenVals =
    replaceBuffer GL.ArrayBuffer (valFunc global) lenVals
inReplaceBuffer (InVec2 valFunc _) global lenVals =
    replaceBuffer GL.ArrayBuffer (valFunc global) lenVals
inReplaceBuffer (InVec3 valFunc _) global lenVals =
    replaceBuffer GL.ArrayBuffer (valFunc global) lenVals
inReplaceBuffer (InVec4 valFunc _) global lenVals =
    replaceBuffer GL.ArrayBuffer (valFunc global) lenVals
inReplaceBuffer (InMat4 _ _) _ _ =
    error "Primitive.inReplaceBuffer: InMat4"
inReplaceBuffer (InBool _ _) _ _ =
    error "Primitive.inReplaceBuffer: InBool"

inLength :: In t -> t -> Int
inLength (InFloat valFunc _) = length . valFunc
inLength (InInt valFunc _) = length . valFunc
inLength (InVec2 valFunc _) = length . valFunc
inLength (InVec3 valFunc _) = length . valFunc
inLength (InVec4 valFunc _) = length . valFunc
inLength (InBool valFunc _) = length . valFunc
inLength (InMat4 valFunc _) = length . valFunc

inDescriptor :: In t -> (GL.GLint, GL.DataType)
inDescriptor InFloat{} = (1, GL.Float)
inDescriptor InInt{} = (1, GL.Int)
inDescriptor InBool{} = (1, GL.Int)
inDescriptor InVec2{} = (2, GL.Float)
inDescriptor InVec3{} = (3, GL.Float)
inDescriptor InVec4{} = (4, GL.Float)
inDescriptor InMat4{} = (16, GL.Float)

inName :: In t -> B.ByteString
inName (InFloat _ name) = name
inName (InInt _ name) = name
inName (InBool _ name) = name
inName (InVec2 _ name) = name
inName (InVec3 _ name) = name
inName (InVec4 _ name) = name
inName (InMat4 _ name) = name

bindAttrib :: t -> AttribGPU t -> IO ()
bindAttrib global (AttribGPU buffer updateFunc location descriptor _) = do
    updateFunc global
    GL.vertexAttribArray location $= GL.Enabled
    GL.bindBuffer GL.ArrayBuffer $= Just buffer
    GL.vertexAttribPointer location $= (GL.ToFloat, descriptor)

-- = Dealing with Uniforms

createUniformList :: GL.Program -> [Uniform t] -> IO [UniformGPU t]
createUniformList prog = mapM (getUniformInfo prog)

getUniformInfo :: GL.Program -> Uniform t -> IO (UniformGPU t)
getUniformInfo prog u = do
    let name = uniName u

    location <- GL.get $ GL.uniformLocation prog (C.unpack name)
    let updateFunc = uniBind u location

    return $ UniformGPU location updateFunc

uniBind :: Uniform t -> GL.UniformLocation -> t -> IO ()
uniBind (UniformFloat valueFunc _) location global =
    GL.uniform location $= GL.Index1 (valueFunc global)
uniBind (UniformInt valueFunc _) location global =
    GL.uniform location $=
        GL.Index1 (fromIntegral $ valueFunc global :: GL.GLint)
uniBind (UniformBool valueFunc _) location global =
    GL.uniform location $= GL.Index1 (boolToGLuint $ valueFunc global)
uniBind (UniformVec2 valueFunc _) location global =
    let x :. y :. () = valueFunc global
    in GL.uniform location $= GL.Vertex2 x y
uniBind (UniformVec3 valueFunc _) location global =
    let x :. y :. z :. () = valueFunc global
    in GL.uniform location $= GL.Vertex3 x y z
uniBind (UniformVec4 valueFunc _) location global =
    let x :. y :. z :. w :. () = valueFunc global
    in GL.uniform location $= GL.Vertex4 x y z w
uniBind (UniformMat4 valueFunc _) (GL.UniformLocation location) global =
    withArray (matToList $ valueFunc global) $ \ptr ->
        glUniformMatrix4fv location 1 1 ptr
    --withArray (matToGLFormat $ valueFunc global) $ \ptr ->
    --    glUniformMatrix4fv location 1 0 ptr
        --GL.uniformv location 16 ptr

-- | Translate from Vec's format (row-major) to
--   OpenGL format (column-major).
matToGLFormat :: Mat44 GL.GLfloat -> [GL.GLfloat]
matToGLFormat = toRowMajor . matToLists
  where
    toRowMajor [(a:as), (b:bs), (c:cs), (d:ds)] =
        a : b : c : d : toRowMajor [as, bs, cs, ds]
    toRowMajor [[],[],[],[]] = []
    toRowMajor _ = error "Primitive.matToGLFormat"

boolToGLuint :: Bool -> GL.GLuint
boolToGLuint True = 1
boolToGLuint False = 0

uniName :: Uniform t -> B.ByteString 
uniName (UniformFloat _ name) = name
uniName (UniformInt _ name) = name
uniName (UniformBool _ name) = name
uniName (UniformVec2 _ name) = name
uniName (UniformVec3 _ name) = name
uniName (UniformVec4 _ name) = name
uniName (UniformMat4 _ name) = name

bindUniform :: t -> UniformGPU t -> IO ()
bindUniform global (UniformGPU _ bindFunc) = bindFunc global

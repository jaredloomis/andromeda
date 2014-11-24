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
module Andromeda.Monad.Primitive where

import Control.Monad (unless, when)
import Control.Applicative ((<$>))
import Data.Monoid ((<>), mempty)
import Data.Proxy

import Foreign (Word8)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Marshal.Array (withArray, withArrayLen)
import Foreign.Storable (Storable, sizeOf)

import Data.Vec
    ((:.)(..), Vec3, Mat44, matToLists, matToList)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C


import qualified Codec.Picture as Juicy
import qualified Codec.Picture.Types as JTypes
import qualified Data.Vector.Storable as V (unsafeWith)

import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL.Raw (glUniformMatrix4fv)
import qualified Graphics.UI.GLFW as GLFW

import Andromeda.Monad.Type
import Andromeda.Monad.GLSL hiding (($=))
import Andromeda.Monad.GPU

---------------------
-- Default actions --
---------------------

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
    GL.clearColor $= GL.Color4 0 0 0 1

    -- Enables clearing of the depth buffer
    GL.clearDepth $= 1
    -- Allow depth testing (3D)
    -- Tells OpenGL how to deal with overlapping shapes
    GL.depthFunc $= Just GL.Less

    -- Do not render the backs of faces. Increases performance.
    GL.cullFace $= Just GL.Back

    -- Call resize function.
    (w, h) <- GLFW.getFramebufferSize win
    resizeScene win w h 

resizeScene :: GLFW.WindowSizeCallback
-- Prevent divide by 0
resizeScene win w 0 = resizeScene win w 1
resizeScene _ width _ = do
    let height' = heightFromWidth width

    -- Make viewport the same size as the window.
    GL.viewport $= (GL.Position 0 0,
                    GL.Size (fromIntegral width) $ height')

getViewportSize :: GLFW.Window -> IO (GL.GLsizei, GL.GLsizei)
getViewportSize win = do
    (width, _) <- GLFW.getFramebufferSize win
    return (fromIntegral width, heightFromWidth width)

heightFromWidth :: Int -> GL.GLsizei
heightFromWidth width =
    floor $ fromIntegral width / (8 / 6 :: GL.GLfloat)

cleanup :: ShaderProgram t -> IO ()
cleanup (ShaderProgram program _ attrs _) = do
    GL.deleteObjectName program
    mapM_ (\(AttribGPU buffer _ _ _ _) -> GL.deleteObjectName buffer) attrs

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


--------------------
-- ShaderGalaxies --
--------------------

data ShaderUniverse t =
    ShaderUniverse [ShaderGalaxy t] [ShaderProgram FBO]

allGalaxies :: ShaderUniverse t -> [ShaderGalaxy t]
allGalaxies (ShaderUniverse xs _) = xs

allPostPrograms :: ShaderUniverse t -> [ShaderProgram FBO]
allPostPrograms (ShaderUniverse _ xs) = xs

hasPostShaders :: ShaderUniverse t -> Bool
hasPostShaders (ShaderUniverse _ []) = False
hasPostShaders _ = True

data ShaderGalaxy t =
    PureGalaxy (ShaderProgram t) (t -> t) t
  | MonadicGalaxy (ShaderProgram t) (t -> IO t) t

(-|>) :: [ShaderGalaxy t] -> [ShaderProgram FBO] -> ShaderUniverse t
(-|>) = ShaderUniverse

mainLoop ::
    GLFW.Window ->
    ShaderUniverse t ->
    IO ()
mainLoop win univ = do
    if not . hasPostShaders $ univ
        then loop univ
    else do
        size <- getViewportSize win
        fbo <- makeFramebuffer size
        loopPP fbo univ
    mapM_ cleanupGalaxy $ allGalaxies univ
    GLFW.destroyWindow win
    GLFW.terminate
  where
    loop (ShaderUniverse galaxies posts) = do
        GL.clear [GL.ColorBuffer, GL.DepthBuffer]
        galaxies' <- mapM drawGalaxy galaxies
        endFrame
        shouldClose <- GLFW.windowShouldClose win
        unless shouldClose $
            loop $ ShaderUniverse galaxies' posts

    drawGalaxy (PureGalaxy prog updateFunc global) = do
        drawProgram prog global
        return . PureGalaxy prog updateFunc $ updateFunc global
    drawGalaxy (MonadicGalaxy prog updateFunc global) = do
        drawProgram prog global
        MonadicGalaxy prog updateFunc <$> updateFunc global

    loopPP fbo@(FBO fbuf size _) (ShaderUniverse galaxies posts) = do
        GL.bindFramebuffer GL.Framebuffer $= fbuf
        GL.viewport $= (GL.Position 0 0, size)
        GL.clear [GL.ColorBuffer, GL.DepthBuffer]

        galaxies' <- mapM drawGalaxy galaxies

        GL.bindFramebuffer GL.Framebuffer $= GL.defaultFramebufferObject
        GL.clear [GL.ColorBuffer, GL.DepthBuffer]
        drawPosts fbo posts fbo

        endFrame
        shouldClose <- GLFW.windowShouldClose win
        unless shouldClose $
            loopPP fbo $ ShaderUniverse galaxies' posts

    endFrame = do
        GLFW.swapBuffers win
        GLFW.pollEvents

    cleanupGalaxy (PureGalaxy prog _ _) = cleanup prog
    cleanupGalaxy (MonadicGalaxy prog _ _) = cleanup prog

------------------------------------------
-- Different representations of Shaders --
------------------------------------------

data Shader (p :: GL.ShaderType) t =
    Shader (Proxy p) (ShaderM p t ())
  | FromBS (Proxy p) (GLSLInfo t) B.ByteString
  | FromFile (Proxy p) (GLSLInfo t) FilePath

type ShaderSequence t = [WrappedShader t]
data WrappedShader t =
    forall p. (ShaderTypeVal (Proxy p)) =>
        Wrapped (Shader p t)

(-&>) :: ShaderTypeVal (Proxy p) =>
    Shader p t -> ShaderSequence t -> ShaderSequence t
(-&>) shader1 wrappedOnes =
    Wrapped shader1 : wrappedOnes
infixr 5 -&>

lastly :: ShaderTypeVal (Proxy p) =>
    Shader p t -> [WrappedShader t]
lastly = return . Wrapped

data ShaderProgram t =
    ShaderProgram
        GL.Program
        (GLSLInfo t)
        [AttribGPU t]
        [UniformGPU t]

class ShaderTypeVal a where
    typeVal :: a -> GL.ShaderType
instance ShaderTypeVal (Proxy GL.VertexShader) where
    typeVal = const GL.VertexShader
instance ShaderTypeVal (Proxy GL.TessControlShader) where
    typeVal = const GL.TessControlShader
instance ShaderTypeVal (Proxy GL.TessEvaluationShader) where
    typeVal = const GL.TessEvaluationShader
instance ShaderTypeVal (Proxy GL.GeometryShader) where
    typeVal = const GL.GeometryShader
instance ShaderTypeVal (Proxy GL.FragmentShader) where
    typeVal = const GL.FragmentShader
instance ShaderTypeVal (Proxy GL.ComputeShader) where
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
    -- Use shader program.
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
    --mapM_ (GL.attachShader program) shaders
    mapM_ (\s -> GL.attachShader program s >> GL.deleteObjectName s) shaders
    GL.linkProgram program
    return program

compileAll :: ShaderSequence t -> IO [GL.Shader]
compileAll (Wrapped current : rest) = do
    firstPrograms <- compile current
    otherPrograms <- compileAll rest
    return $ firstPrograms : otherPrograms
compileAll [] = return []

compile :: ShaderTypeVal (Proxy p) =>
            Shader p t -> IO GL.Shader
compile (Shader proxy glsl) =
    let code = generateGLSL glsl
        shaderType = typeVal proxy
    in compileShader shaderType code
compile (FromBS proxy _ code) =
    let shaderType = typeVal proxy
    in compileShader shaderType code
compile (FromFile proxy _ file) =
    let shaderType = typeVal proxy
    in compileShader shaderType =<<
        B.readFile file

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
    let GLSLInfo ins uniforms outs = evalShaderM glsl
        ins' = case typeVal proxy of
            GL.VertexShader -> ins
            _               -> []
        outs' = case typeVal proxy of
            GL.FragmentShader -> outs
            _                 -> []
    in GLSLInfo ins' uniforms outs' <> gatherInfo shaders
gatherInfo (Wrapped (FromBS proxy info _) : shaders) =
    let GLSLInfo ins uniforms outs = info
        (ins', outs') = case typeVal proxy of
            GL.VertexShader -> (ins, outs)
            _               -> ([], [])
    in GLSLInfo ins' uniforms outs' <> gatherInfo shaders
gatherInfo (Wrapped (FromFile proxy info _) : shaders) =
    let GLSLInfo ins uniforms outs = info
        (ins', outs') = case typeVal proxy of
            GL.VertexShader -> (ins, outs)
            _               -> ([], [])
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
inMkBuffer InMat4{} _ =
    error "Primitive.inMkBuffer Given Mat4. Idk what to do."
inMkBuffer InNone{} _ =
    error "Primitive.inMkBuffer Given InNone. Idk what to do."

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
inReplaceBuffer InMat4{} _ _ =
    error "Primitive.inReplaceBuffer: InMat4"
inReplaceBuffer InNone{} _ _ =
    error "Primitive.inReplaceBuffer: InNone"

inLength :: In t -> t -> Int
inLength (InFloat valFunc _) = length . valFunc
inLength (InInt valFunc _) = length . valFunc
inLength (InVec2 valFunc _) = length . valFunc
inLength (InVec3 valFunc _) = length . valFunc
inLength (InVec4 valFunc _) = length . valFunc
inLength (InMat4 valFunc _) = length . valFunc
inLength InNone{} = error "Primitive.inLength: Given InNone."

inDescriptor :: In t -> (GL.GLint, GL.DataType)
inDescriptor InFloat{} = (1, GL.Float)
inDescriptor InInt{} = (1, GL.Int)
inDescriptor InVec2{} = (2, GL.Float)
inDescriptor InVec3{} = (3, GL.Float)
inDescriptor InVec4{} = (4, GL.Float)
inDescriptor InMat4{} = (16, GL.Float)
inDescriptor InNone{} = error "Primitive.inDescriptor: Given InNone."

inName :: In t -> B.ByteString
inName (InFloat _ name) = name
inName (InInt _ name) = name
inName (InVec2 _ name) = name
inName (InVec3 _ name) = name
inName (InVec4 _ name) = name
inName (InMat4 _ name) = name
inName (InNone name) = name

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
uniBind (UniformSampler2D valueFunc _) location global = do
    let Sampler2DInfo textureObject textureUnit = valueFunc global
        GL.TextureUnit textureId = textureUnit
    GL.activeTexture $= textureUnit
    GL.textureBinding GL.Texture2D $= Just textureObject
    GL.uniform location $= GL.Index1 (fromIntegral textureId :: GL.GLint)
uniBind (UniformSampler3D valueFunc _) location global = do
    let Sampler3DInfo textureObject textureUnit = valueFunc global
        GL.TextureUnit textureId = textureUnit
    GL.activeTexture $= textureUnit
    GL.textureBinding GL.Texture3D $= Just textureObject
    GL.uniform location $= GL.Index1 (fromIntegral textureId :: GL.GLint)

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
uniName (UniformVec2 _ name) = name
uniName (UniformVec3 _ name) = name
uniName (UniformVec4 _ name) = name
uniName (UniformMat4 _ name) = name
uniName (UniformSampler2D _ name) = name
uniName (UniformSampler3D _ name) = name

bindUniform :: t -> UniformGPU t -> IO ()
bindUniform global (UniformGPU _ bindFunc) = bindFunc global

-- | Load an image and turn it into something OpenGL can use.
juicyLoadTexture :: FilePath -> IO GL.TextureObject
juicyLoadTexture file = do
    (w, h, ptr) <- juicyLoadImageRaw file

    texObject <- GL.genObjectName
    GL.textureBinding GL.Texture2D $= Just texObject

    GL.texImage2D GL.Texture2D
        -- No proxy.
        GL.NoProxy
        -- Mipmaps.
        0
        -- Use RGB format.
        GL.RGB'
        -- Size of image.
        (GL.TextureSize2D w h)
        -- No borders
        0
        -- The pixel data.
        (GL.PixelData GL.RGB GL.UnsignedByte ptr)

    --GL.textureBinding GL.Texture2D $= Just texObject
    GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
    GL.textureWrapMode GL.Texture2D GL.S $= (GL.Mirrored, GL.ClampToEdge)
    GL.textureWrapMode GL.Texture2D GL.T $= (GL.Mirrored, GL.ClampToEdge)

    return texObject

-- TODO: add support for all (most) colorspaces / formats.
juicyLoadImageRaw :: FilePath -> IO (GL.GLint, GL.GLint, Ptr Word8)
juicyLoadImageRaw file = do
    image <- Juicy.readImage file

    case image of
        Left err -> error err

        Right (Juicy.ImageRGB8 (Juicy.Image w h dat)) ->
            V.unsafeWith dat $ \ptr ->
                return (fromIntegral w, fromIntegral h, ptr)
        Right (Juicy.ImageYCbCr8 img) ->
            let (Juicy.Image w h dat) =
                    JTypes.convertImage img :: Juicy.Image Juicy.PixelRGB8
            in V.unsafeWith dat $ \ptr ->
                return (fromIntegral w, fromIntegral h, ptr)
        Right (Juicy.ImageCMYK8 img) ->
            let (Juicy.Image w h dat) =
                    JTypes.convertImage img :: Juicy.Image Juicy.PixelRGB8
            in V.unsafeWith dat $ \ptr ->
                return (fromIntegral w, fromIntegral h, ptr)
        _ -> error $
            "Engine.Graphics.Texture.juicyLoadImage:"
                ++ "bad image colorspace or format."

-----------------
-- Framebuffer --
-----------------

data FBO = FBO
    GL.FramebufferObject GL.Size GL.TextureObject
  deriving (Show)

fboSampler :: FBO -> Sampler2D
fboSampler (FBO _ _ texObj) = Sampler2DInfo texObj $ GL.TextureUnit 0

drawProgramWithPP :: ShaderProgram t -> [ShaderProgram t] -> FBO -> t -> IO ()
drawProgramWithPP mainSh postSh fbo@(FBO fbuf size _) global = do
    GL.bindFramebuffer GL.Framebuffer $= fbuf
    GL.viewport $= (GL.Position 0 0, size)
    drawProgram mainSh global

    GL.bindFramebuffer GL.Framebuffer $= GL.defaultFramebufferObject    
    drawPosts fbo postSh global

drawPosts :: FBO -> [ShaderProgram t] -> t -> IO ()
drawPosts fbo (shader : y : xs) global = do
    drawPost fbo shader global
    drawPosts fbo (y:xs) global
drawPosts fbo [shader] global = do
    GL.bindFramebuffer GL.Framebuffer $= GL.defaultFramebufferObject
    drawPost fbo shader global
drawPosts _ _ _ = return ()

drawPost :: FBO -> ShaderProgram t -> t -> IO ()
drawPost (FBO _ _ texObj) (ShaderProgram prog _ attribs unis) global = do
    -- Use shader program.
    GL.currentProgram $= Just prog

    -- Activate FBO.
    GL.activeTexture $= GL.TextureUnit 0
    GL.textureBinding GL.Texture2D $= Just texObj

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

makeFramebuffer :: (GL.GLsizei, GL.GLsizei) -> IO FBO
makeFramebuffer (winW, winH) = do
    -- Create texture (color buffer).
    fbTex <- GL.genObjectName
    GL.textureBinding GL.Texture2D $= Just fbTex
    -- Specify texture filtering and wraping.
    GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
    GL.textureWrapMode GL.Texture2D GL.S $= (GL.Mirrored, GL.ClampToEdge)
    GL.textureWrapMode GL.Texture2D GL.T $= (GL.Mirrored, GL.ClampToEdge)
    -- Fill the texture with nothing, give OpenGL
    -- its specs.
    GL.texImage2D GL.Texture2D
        GL.NoProxy
        0
        GL.RGB'
        (GL.TextureSize2D winW winH)
        0
        (GL.PixelData GL.RGB GL.UnsignedByte nullPtr)
    GL.textureBinding GL.Texture2D $= Nothing

    -- Create depth buffer.
    depthRenderbuffer <- GL.genObjectName
    GL.bindRenderbuffer GL.Renderbuffer $= depthRenderbuffer
    let rbufSize = GL.RenderbufferSize winW winH
    GL.renderbufferStorage GL.Renderbuffer GL.DepthComponent' rbufSize
    GL.bindRenderbuffer GL.Renderbuffer $= GL.noRenderbufferObject

    -- Create an FBO and bind it.
    fbName <- GL.genObjectName
    GL.bindFramebuffer GL.Framebuffer $= fbName
    -- Set Framebuffer's depth buffer.
    GL.framebufferRenderbuffer GL.Framebuffer
        GL.DepthAttachment GL.Renderbuffer depthRenderbuffer
    -- Set Framebuffer's texture.
    GL.framebufferTexture2D GL.Framebuffer (GL.ColorAttachment 0)
                            GL.Texture2D fbTex 0

    GL.drawBuffers $= [GL.FBOColorAttachment 0]

    fbufStatus <- GL.get $ GL.framebufferStatus GL.Framebuffer
    when (fbufStatus /= GL.Complete) $ do
        error $ "frambufferStatus returned a value " ++
                "other than 'Complete'. Status returned: '" ++
                show fbufStatus ++ "'."

    return $ FBO fbName (GL.Size winW winH) fbTex

screenBufferData :: [Vec3 GL.GLfloat]
screenBufferData =
    [(-1.0) :. (-1.0) :. 0.0 :. (),
     1.0    :. (-1.0) :. 0.0 :. (),
     (-1.0) :. 1.0    :. 0.0 :. (),
     (-1.0) :. 1.0    :. 0.0 :. (),
     1.0    :. (-1.0) :. 0.0 :. (),
     1.0    :. 1.0    :. 0.0 :. ()]
{-# INLINE screenBufferData #-}

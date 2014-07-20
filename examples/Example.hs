{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Example where

import Control.Monad
import Control.Applicative ((<$>))
import Data.Vec hiding (pack)
import qualified Graphics.Rendering.OpenGL as GL
import Data.Time.Clock (utctDayTime, getCurrentTime)
import qualified Graphics.UI.GLFW as GLFW

import Language.GLSL.Monad

data Object = Object (Vec3 GL.GLfloat) [Vec3 GL.GLfloat] GL.GLfloat

getObjVerts :: Object -> [Vec3 GL.GLfloat]
getObjVerts (Object _ verts _) = verts

getObjPos :: Object -> Vec3 GL.GLfloat
getObjPos (Object pos _ _) = pos

getObjMat :: Object -> Mat44 GL.GLfloat
getObjMat (Object pos _ _) = translation pos

getObjTime :: Object -> GL.GLfloat
getObjTime (Object _ _ time) = time

updateObj :: Object -> IO Object
updateObj (Object _ verts _) = do
    time <- realToFrac . utctDayTime <$> getCurrentTime
    return $ Object (sin time :. cos time :. 0 :. ()) verts time

testObj :: Object
testObj = Object
        (0 :. 0 :. 0 :. ())
        [(-1) :. (-1) :. 0 :. (),
         1 :. (-1) :. 0 :. (),
         0 :. 1 :. 0 :. ()]
        0

main :: IO ()
main = do
    let obj = testObj

    win <- openWindow
    initGL win
    prog <- simpleProgram
    loop win prog obj
  where
    loop win prog obj = do
        drawProgram prog obj
        GLFW.swapBuffers win
        GLFW.pollEvents
        shouldClose <- GLFW.windowShouldClose win
        newObj <- updateObj obj
        unless shouldClose $
            loop win prog newObj

simpleProgram :: IO (ShaderProgram Object)
simpleProgram = 
    createProgram
        [Wrapped simpleVert, Wrapped simpleFrag]
        testObj

simpleVert :: Shader GL.VertexShader Object
simpleVert = Shader STProxy $ do
    version "440 core"
    position <- layoutIn ["location=0"] vec3 "position" getObjVerts
    let position4 = "vec4" .$ [pack position, pack $ constant float "1.0"]
            :: Expression "vec4"

    modelMatrix <- uniform mat4 "modelMatrix" getObjMat

    glPosition #= modelMatrix .* position4

simpleFrag :: Shader GL.FragmentShader Object
simpleFrag = Shader STProxy $ do
    version "440 core"
    time <- uniform float "time" getObjTime
    color <- out vec3 "color"
    color #= constant vec3 "vec3(sin(time),cos(time),tan(time))"

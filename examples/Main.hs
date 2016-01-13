module Main where

import Data.Vec ((:.)(..), Vec3, Vec4)

import qualified Data.Vector.Storable as V

import qualified Graphics.Rendering.OpenGL.GL as GL

import Andromeda.Simple.Expr
import Andromeda.Simple.Type
import Andromeda.Simple.StdLib
import Andromeda.Simple.GLSL

import Andromeda.Simple.Render.Mesh
import Andromeda.Simple.Render.VertexBuffer
import Andromeda.Simple.Render.Compile

main :: IO ()
main = do
    win <- openWindow
    initGL win

    putStrLn $ toGLSL vertShader
    putStrLn $ toGLSL fragShader
    prog <- addMesh myMesh =<< compile vertShader fragShader

    mainLoop win prog

glPosition :: Expr (Vec4 Float)
glPosition = fetch "vertex" (Vec3T SFloat) +: 1

vertShader :: Statement ()
vertShader = do
    "gl_Position" =: glPosition
    out "fragPos" glPosition

outColor :: Expr (Vec3 Float)
outColor =
    let fragPos = fetch   "fragPos" (Vec4T SFloat)
        inColor = uniform "inColor" (Vec3T SFloat)
    in vzipWith avg inColor fragPos
  where
    avg x y = (x + y) / 2

fragShader :: Statement ()
fragShader = out "color" outColor

myMesh :: Mesh
myMesh = Mesh
    [("vertex",  MeshAttribute $ V.fromList triangle),
     ("inColor", MeshUniform   $ Uniform color)] Triangles

triangle :: [Vec3 Float]
triangle = [(-1):.(-1):.0:.(),
              1 :.(-1):.0:.(),
              0 :.  1 :.0:.()]

color :: GL.Vertex3 GL.GLfloat
color = GL.Vertex3 0.5 0.5 0.5

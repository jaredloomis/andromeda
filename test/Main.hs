{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Data.Vec ((:.)(..), Vec2, Vec3, Vec4)

import qualified Data.Vector.Storable as V

{-
import Andromeda.Lambda.Expr
import Andromeda.Lambda.Type
import Andromeda.Lambda.StdLib
import Andromeda.Lambda.Shader
import Andromeda.Lambda.Utils
import Andromeda.Lambda.VertexBuffer
-}

import Andromeda.Simple.Expr
import Andromeda.Simple.Type
import Andromeda.Simple.GLSL
import Andromeda.Simple.Var

import Andromeda.Simple.Render.Mesh
import Andromeda.Simple.Render.VertexBuffer
import Andromeda.Simple.Render.Compile

main :: IO ()
main = do
    win <- openWindow
    initGL win

    prog <- addMesh myMesh =<< compile simpleV simpleF

    mainLoop win prog

simpleV :: Statement
simpleV =
    let expr =
            (Lit (Native "vec4") :: Expr (Vec3 Float -> Float -> Vec4 Float))
            :$ Lit (Fetch "vertex" (Vec3T SFloat)) :$ 1
    in AssignS "gl_Position" expr

simpleF :: Statement
simpleF = OutS "color" (Lit (Native "vec3(1,0,0)") :: Expr (Vec3 Float))

myMesh :: Mesh
myMesh = Mesh [("vertex", MeshAttribute $ V.fromList triangle)] Triangles

triangle :: [Vec3 Float]
triangle = [(-1):.(-1):.0:.(),
              1 :.(-1):.0:.(),
              0 :.  1 :.0:.()]

{-
main :: IO ()
main = do
    win <- openWindow
    initGL win

    prog <- compile
        (AssignS "gl_Position" simpleV)
        (OutS    "outColor"    simpleF)

    mainLoop win prog

updateTime :: Float -> Input (Float, Vec3 Float)
updateTime i = UniformInput i `PairI` InInput triangle

triangle :: [Vec3 Float]
triangle = [(-1):.(-1):.0:.(),
              1 :.(-1):.0:.(),
              0 :.  1 :.0:.()]

pixelV :: Expr (Vec3 Float -> Vec2 Float -> (Vec3 Float, Vec2 Float))
pixelV = lam pair

pixelF :: Expr (Sampler 2 -> Vec2 Float -> (Vec4 Float, Int))
pixelF = lam $ \renderedTexture textureCoord ->
    let (w, h)  = (flt 800, flt 600)
        (pw,ph) = (flt  10, flt  10)
        dx      = pw * (1 / w)
        dy      = ph * (1 / h)
        cx      = dx * (floorG (textureCoord ! X) / dx)
        cy      = dy * (floorG (textureCoord ! Y) / dy)
        tex     = texture renderedTexture (cx +-+ cy)
                    :: Expr (Vec4 Float)
    in pair tex 10

simpleV :: Expr (Vec4 Float)
simpleV = inn "vertex" triangle +-+ 1

simpleF :: Expr (Vec3 Float)
simpleF = flt 1 +-+ flt 0 +-+ flt 0
-}

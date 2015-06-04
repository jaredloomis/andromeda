{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Data.Vec ((:.)(..), Vec2, Vec3, Vec4)
--import qualified Data.Vec as V

import qualified Graphics.Rendering.OpenGL as GL

import Andromeda.Lambda.Expr
import Andromeda.Lambda.Type
import Andromeda.Lambda.StdLib
import Andromeda.Lambda.Shader
import Andromeda.Lambda.Utils
import Andromeda.Lambda.GLSL
import Andromeda.Lambda.HasAttr
import Andromeda.Lambda.NatR

main :: IO ()
main = do
    win <- openWindow
    initGL win
{-
    prog <- compile minV minF
        (InInput triangle)
        (const $ InInput triangle)
    mainLoop win prog () id
-}
    prog <- compile simpleV simpleF
        (UniformInput 0 `PairI` InInput triangle)
        updateTime

    GL.get GL.errors >>= print

    mainLoop win prog (0::Float) (+0.01)

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

newV :: Expr (Vec4 Float, Int)
newV =
    let renderedTexture = Lit $ LitUnif (Sampler (TS $ TS TZ) undefined) guessTy "renderedTexture"
        textureCoord    = Lit $ LitIn   [0,1,2] guessTy "textureCoord" :: Expr (Vec2 Float)
        (w, h)  = (flt 800, flt 600)
        (pw,ph) = (flt  10, flt  10)
        dx      = pw * (1 / w)
        dy      = ph * (1 / h)
        cx      = dx * (floorG (textureCoord ! X) / dx)
        cy      = dy * (floorG (textureCoord ! Y) / dy)
        tex     = texture renderedTexture (cx +-+ cy)
                    :: Expr (Vec4 Float)
    in pair tex 10

vertexShader :: Expr (Vec3 Float -> Vec4 Float)
vertexShader = Lam (+-+ (1 :: Expr Float))

fragmentShader :: Expr (Vec4 Float)
fragmentShader = 1 +-+ flt 0 +-+ flt 0 +-+ flt 1

minV :: Expr (Vec3 Float -> (Vec4 Float, ()))
minV = Lam $ \pos ->
    pair (pos +-+ 1.0) bottom

minF :: Expr (() -> Vec3 Float)
minF = Lam $ \_ -> flt 1.0 +-+ flt 0.0 +-+ 0.0


simpleV :: Expr ((Float, Vec3 Float) -> (Vec4 Float, Float))
simpleV = Lam $ \p ->
    let (offs, expr) = unPair p
    in pair (expr +-+ 1.0) offs
--        xyzE = (expr ! X & Y) +-+ (-offs)
--        mat  = lit . Mat4 $ V.perspective 0.1 100 45 (800/600)
--    in pair :$ (mat #* (xyzE +-+ 1.0)) :$ offs

simpleF :: Expr (Float -> Vec3 Float)
simpleF = Lam $ \x -> (1 :: Expr Float) +-+ (0 :: Expr Float) +-+ 0
--    sin x +-+ cos x +-+ (cos . sin) x

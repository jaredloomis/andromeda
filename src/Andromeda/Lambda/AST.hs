{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where --Language.GLSL.Monad.AST where

import GHC.TypeLits

import Data.Vec ((:.)(..), Vec2, Vec3, Vec4, Mat44)
import qualified Data.Vec as Vec

import Andromeda.Lambda.Expr
import Andromeda.Lambda.StdLib
import Andromeda.Lambda.Shader
import Andromeda.Lambda.Utils
import Andromeda.Lambda.GLSL

main :: IO ()
main = do
    win <- openWindow
    initGL win
    prog <- compile simpleV simpleF
        (UniformInput 33 `PairI` InInput triangle)
        (const $ UniformInput 33 `PairI` InInput triangle)
        --(\i -> InInput $ map (+(i:.0:.0:.())) triangle)
    mainLoop win prog (0::Float) (+0.001)

triangle :: [Vec3 Float]
triangle = [(-1):.(-1):.0:.(),
              1 :.(-1):.0:.(),
              0 :.  1 :.0:.()]

pixelV :: Expr (Vec3 Float -> Vec2 Float -> (Vec3 Float, Vec2 Float))
pixelV = pair

pixelF :: Expr (Sampler 2 -> Vec2 Float -> (Vec4 Float, Int))
pixelF = lam $ \renderedTexture textureCoord ->
    let vec2    = Lit (Native "vec2") :: Expr (a -> a -> Vec2 a)
        floorG  = Lit (Native "floorG")
        texture = Lit (Native "texture")
        (w, h)  = (800, 600) :: (Expr Float, Expr Float)
        (pw,ph) = (10, 10) :: (Expr Float, Expr Float)
        dx      = pw * (1 / w)
        dy      = ph * (1 / h)
        cx      = dx * (floorG :$ ((textureCoord ! X) / dx))
        cy      = dy * (floorG :$ ((textureCoord ! Y) / dy))
        tex     = texture :$ renderedTexture :$ (vec2 :$ cx :$ cy)
                    :: Expr (Vec4 Float)
    in pair :$ tex :$ 10

vertexShader :: Expr (Vec3 Float -> Vec4 Float)
vertexShader = Lam (+-+ (1 :: Expr Float))

fragmentShader :: Expr (Vec4 Float)
fragmentShader = 1 +-+ flt 0 +-+ flt 0 +-+ flt 1

flt :: Expr Float -> Expr Float
flt = id



--test = comp' simpleV simpleF () (const [0:.0:.0:.()])

{-
simpleV :: Expr (Vec3 Float -> (Vec4 Float, Vec3 Float))
simpleV = Lam $ \expr ->
    let xyzE = Lit (FieldAccess "xyz") :$ expr
        vec4 = Lit (Native "vec4") :: Expr (Vec3 Float -> Float -> Vec4 Float)
    in pair :$ (vec4 :$ xyzE :$ 1.0) :$ xyzE
-}

simpleV :: Expr ((Float, Vec3 Float) -> (Vec4 Float, Vec3 Float))
simpleV = Lam $ \p -> --Lam $ \offs -> Lam $ \expr ->
    let (offs, expr) = unPair p
        xyzE = expr ! X & Y & Z
        newE = xyzE ! X & Y +-+ (xyzE ! Z) + offs
    in pair :$ (newE +-+ 1.0) :$ xyzE

simpleF :: Expr (Vec3 Float -> Vec3 Float)
simpleF = Lam . const $ Lit (Literal (1:.0:.0:.()))

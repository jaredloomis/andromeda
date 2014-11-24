{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where --Language.GLSL.Monad.AST where

import GHC.TypeLits

import Data.Vec ((:.)(..), Vec2, Vec3, Vec4, Mat44)
import qualified Data.Vec as Vec

import Expr
import Utils
import StdLib
import Shader

main :: IO ()
main = do
    win <- openWindow
    initGL win
    prog <- compile simpleV simpleF triangle (\i -> map (+(i:.0:.0:.())) triangle)
    mainLoopSF win prog (0::Float) (+0.001)

triangle :: [Vec3 Float]
triangle = [(-1):.(-1):.0:.(),
              1 :.(-1):.0:.(),
              0 :.  1 :.0:.()]

pixelV :: Expr (Vec3 Float -> Vec2 Float -> (Vec3 Float, Vec2 Float))
pixelV = pair {-lam $ \position textureCoord ->
    pair :$ position :$ textureCoord-}

pixelF :: Expr (Sampler 2 -> Vec2 Float -> (Vec4 Float, Int))
pixelF = lam $ \renderedTexture textureCoord ->
    let vec2    = Lit (Native "vec2") :: Expr (a -> a -> Vec2 a)
        floorG  = Lit (Native "floorG")
        getX    = Lit (FieldAccess "x")
        getY    = Lit (FieldAccess "y")
        texture = Lit (Native "texture")
        (w, h)  = (800, 600) :: (Expr Float, Expr Float)
        (pw,ph) = (10, 10) :: (Expr Float, Expr Float)
        dx      = pw * (1 / w)
        dy      = ph * (1 / h)
        cx      = dx * (floorG :$ ((getX :$ textureCoord) / dx))
        cy      = dy * (floorG :$ ((getY :$ textureCoord) / dy))
        tex     = texture :$ renderedTexture :$ (vec2 :$ cx :$ cy)
                    :: Expr (Vec4 Float)
    in pair :$ tex :$ 10

simpleV :: Expr (Vec3 Float -> (Vec4 Float, Vec3 Float))
simpleV = Lam $ \expr ->
    let xyzE = Lit (FieldAccess "xyz") :$ expr
        vec4 = Lit (Native "vec4") :: Expr (Vec3 Float -> Float -> Vec4 Float)
    in pair :$ (vec4 :$ xyzE :$ 1.0) :$ xyzE

simpleF :: Expr (Vec3 Float -> Vec3 Float)
simpleF = Lam . const $ Lit (Literal (1:.0:.0:.()))

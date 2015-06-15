{-# LANGUAGE FlexibleInstances #-}
module Andromeda.Simple.GLSL where

import Data.Vec ((:.)(..), Vec2,  Vec3,  Vec4)

import Andromeda.Simple.Util

class GLSL a where
    toGLSL :: a -> String

instance GLSL Int where
    toGLSL = show
instance GLSL Word where
    toGLSL = show
instance GLSL Float where
    toGLSL = show
instance GLSL Bool where
    toGLSL = show

instance GLSL a => GLSL (Vec2 a) where
    toGLSL (x:.y:.()) =
        "vec2(" ++ toGLSL x ++ ", " ++ toGLSL y ++ ")"
instance GLSL a => GLSL (Vec3 a) where
    toGLSL (x:.y:.z:.()) =
        "vec3(" ++ toGLSL x ++ ", " ++ toGLSL y ++ ", " ++
                   toGLSL z ++ ")"
instance GLSL a => GLSL (Vec4 a) where
    toGLSL (x:.y:.z:.w:.()) =
        "vec3(" ++ toGLSL x ++ ", " ++ toGLSL y ++ ", " ++
                   toGLSL z ++ ", " ++ toGLSL w ++ ")"


instance GLSL a => GLSL (Matrix2 a) where
    toGLSL (Matrix2 (x:.y:.())) =
        "mat2(" ++ toGLSL x ++ ", " ++ toGLSL y ++ ")"
instance GLSL a => GLSL (Matrix3 a) where
    toGLSL (Matrix3 (x:.y:.z:.())) =
        "mat3(" ++ toGLSL x ++ ", " ++ toGLSL y ++ ", " ++
                   toGLSL z ++ ")"
instance GLSL a => GLSL (Matrix4 a) where
    toGLSL (Matrix4 (x:.y:.z:.w:.())) =
        "mat4(" ++ toGLSL x ++ ", " ++ toGLSL y ++ ", " ++
                   toGLSL z ++ ", " ++ toGLSL w ++ ")"

---------------------------------
-- Dummy instances for 'Typed' --
---------------------------------

instance GLSL () where
    toGLSL _ = error "toGLSL () is not possible."
instance (GLSL a, GLSL b) => GLSL (a -> b) where
    toGLSL _ = error "toGLSL (a -> b) is not possible."
instance (GLSL a, GLSL b) => GLSL (a, b) where
    toGLSL _ = error "toGLSL (a, b) is not possible."

instance GLSL Sampler1D where
    toGLSL _ = error "toGLSL Sampler1D is not possible."
instance GLSL Sampler2D where
    toGLSL _ = error "toGLSL Sampler1D is not possible."
instance GLSL Sampler3D where
    toGLSL _ = error "toGLSL Sampler1D is not possible."

{-# LANGUAGE FlexibleInstances #-}
module HasGLSL (HasGLSL(..)) where

import GHC.Stack (errorWithStackTrace)
import Data.Word (Word)
import Data.Char (toLower)
import Data.List (intercalate)

import Data.Vec ((:.)(..), Vec2, Vec3, Vec4, Mat22, Mat33, Mat44)
import qualified Data.Vec as V

class HasGLSL a where
    toGLSL :: a -> String

-- Scalars
instance HasGLSL Int where
    toGLSL = show
instance HasGLSL Float where
    toGLSL = show
instance HasGLSL Word where
    toGLSL = show
instance HasGLSL Bool where
    toGLSL = map toLower . show
-- Unit
instance HasGLSL () where
    toGLSL _ = "()"
-- Pair
instance (HasGLSL a, HasGLSL b) => HasGLSL (a, b) where
    toGLSL (l, r) = "(" ++ toGLSL l ++ ", " ++ toGLSL r ++ ")"
-- Vecs
instance HasGLSL a => HasGLSL (Vec2 a) where
    toGLSL (x:.y:.()) =
        "vec2(" ++ toGLSL x ++ ", " ++
                   toGLSL y ++ ")"
instance HasGLSL a => HasGLSL (Vec3 a) where
    toGLSL (x:.y:.z:.()) =
        "vec3(" ++ toGLSL x ++ ", " ++
                   toGLSL y ++ ", " ++
                   toGLSL z ++ ")"
instance HasGLSL a => HasGLSL (Vec4 a) where
    toGLSL (x:.y:.z:.w:.()) =
        "vec4(" ++ toGLSL x ++ ", " ++
                   toGLSL y ++ ", " ++
                   toGLSL z ++ ", " ++
                   toGLSL w ++ ")"
-- Mats
instance HasGLSL a => HasGLSL (Mat22 a) where
    toGLSL mat =
        "mat2(" ++ intercalate ", "
        (map toGLSL (matToGLList mat)) ++ ")"
instance HasGLSL a => HasGLSL (Mat33 a) where
    toGLSL mat =
        "mat3(" ++ intercalate ", "
        (map toGLSL (matToGLList mat)) ++ ")"
instance HasGLSL a => HasGLSL (Mat44 a) where
    toGLSL mat =
        "mat4(" ++ intercalate ", "
        (map toGLSL (matToGLList mat)) ++ ")"

instance HasGLSL (a -> b) where
    toGLSL = errorWithStackTrace
        "toGLSL (a -> b): There isn't actually a way to do this."

matToGLList :: (V.Fold v a, V.Fold m v) => m -> [a]
matToGLList = concat . toRowMajor . V.matToLists

toRowMajor :: [[a]] -> [[a]]
toRowMajor xss =
    map head xss : toRowMajor (map tail xss)

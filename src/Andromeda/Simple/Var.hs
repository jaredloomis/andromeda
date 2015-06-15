module Andromeda.Simple.Var where

import Data.String (IsString(..))

import Andromeda.Simple.GLSL
import Andromeda.Simple.Type

data V a = V String (Type a)

instance Show (V a) where
    show (V n _) = "V " ++ n

instance Typed a => IsString (V a) where
    fromString name = V name guessTy

instance GLSL (V a) where
    toGLSL (V name ty) = toGLSL ty ++ " " ++ name

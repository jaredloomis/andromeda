{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Andromeda.Lambda.Glom where

import GHC.Stack (errorWithStackTrace)
import Data.String (IsString(..))

import Andromeda.Lambda.Type

data Glom f a where
    UnitG :: Glom f ()
    BaseG :: (HasType a, HasGLSL a) => f a -> Glom f a
    PairG :: (HasType a, HasType b, HasGLSL a, HasGLSL b) =>
              Glom f a -> Glom f b -> Glom f (a, b)

-- | A pattern for a Variable. This is how
--   non-native conglomerate types can be
--   expressed in glsl code and given a name.
type Pat a = Glom V a

showBasePat :: Pat a -> String
showBasePat  UnitG          = ""
showBasePat (BaseG (V n _)) = n
showBasePat (l `PairG` _)   = showBasePat l

pat :: forall a. HasType a => String -> Pat a
pat vname = dvy "" (typeOf (undefined :: a))
  where
    dvy :: HasType s => String -> Type s -> Pat s
    dvy _   UnitT      = UnitG
    dvy str (a :*:  b) = dvy (fstP str) a `PairG` dvy (sndP str) b
    dvy _   (_ :->: _) = errorWithStackTrace
        "pat: Can't create a pattern for (:->:)."
    dvy str t          = BaseG $ V (namePath vname str) t

patV :: (HasType a, HasGLSL a) => V a -> Pat a
patV (V vname _) = pat vname

namePath :: String -> String -> String
namePath vname "" = vname
namePath vname p  = vname ++ "_" ++ p

fstP :: String -> String
fstP = ("fst"++)
sndP :: String -> String
sndP = ("snd"++)


-- | A GLSL Variable.
data V a = V String (Type a)

instance Show (V a) where
    show (V n _) = "V " ++ n

instance HasType a => IsString (V a) where
    fromString name = V name (typeOf (undefined :: a))

instance HasGLSL (V a) where
    toGLSL (V name ty) = toGLSL ty ++ " " ++ name

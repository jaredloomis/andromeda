{-# LANGUAGE GADTs #-}
module Andromeda.Lambda.Glom where

import Andromeda.Lambda.Type

data Glom f a where
    UnitG  :: Glom f ()
    BaseG :: (HasType a, HasGLSL a) => f a -> Glom f a
    PairG  :: (HasType a, HasType b, HasGLSL a, HasGLSL b) =>
              Glom f a -> Glom f b -> Glom f (a, b)

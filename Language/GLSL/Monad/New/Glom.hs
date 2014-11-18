{-# LANGUAGE GADTs #-}
module Glom where

import Type
import HasGLSL

data Glom f a where
    UnitG  :: Glom f ()
    BaseG :: (HasType a, HasGLSL a) => f a -> Glom f a
    (:*)  :: (HasType a, HasType b, HasGLSL a, HasGLSL b) =>
              Glom f a -> Glom f b -> Glom f (a, b)
infixr 7 :*

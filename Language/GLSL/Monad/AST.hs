{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
module Language.GLSL.Monad.AST where

import Data.Proxy
import GHC.TypeLits

data Stream (t :: Type) = Stream

data Type =
    GInt
  | GUInt

class CPU a where
    type GPU a :: *
    toGPU :: a -> GPU a


{-
data AST :: ASTs -> * where
    ASTThing :: ASTs -> AST t

--data Nat = S Nat | Z

data ASTs =
    Var Symbol
-}

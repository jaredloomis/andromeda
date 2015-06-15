{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
module Andromeda.Lambda.GPU where

class GPU a where
    type CPU a :: *
    toGPU :: CPU a -> a

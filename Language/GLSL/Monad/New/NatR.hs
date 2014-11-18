{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
module NatR where

import GHC.TypeLits
import Unsafe.Coerce (unsafeCoerce)

-- | Type-level Nat.
data NatR (n :: Nat) where
    TS :: NatR n -> NatR (n+1)
    TZ :: NatR 0

natRToInt :: NatR n -> Int
natRToInt TZ = 0
natRToInt (TS k) = 1 + natRToInt k

intToNatR :: Int -> NatR n
intToNatR i | i <= 0    = unsafeCoerce TZ
            | otherwise = unsafeCoerce $ TS (intToNatR $ i-1)

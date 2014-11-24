{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module Andromeda.Monad.StdLib where

import Data.String (fromString)
import Data.Proxy
import Data.Monoid ((<>))

import Andromeda.Monad.Type
import Andromeda.Monad.GLSL
import Andromeda.Monad.GPU

------------------------
-- Numeric operations --
------------------------

(.+) :: (HasGPUCode a, HasGPUCode b,
         ReadableQ a, ReadableQ b) =>
    a -> b -> Expression (Math (TypeOf a) (TypeOf b))
(.+) left right =
    Expression (paren (getGPUCode left) <> " + " <> paren (getGPUCode right))
                Proxy
infixl 6 .+

(.-) :: (HasGPUCode a, HasGPUCode b,
         ReadableQ a, ReadableQ b) =>
    a -> b -> Expression (Math (TypeOf a) (TypeOf b))
(.-) left right =
    Expression (paren (getGPUCode left) <> " - " <> paren (getGPUCode right))
                Proxy
infixl 6 .-

(.*) :: (HasGPUCode a, HasGPUCode b,
         ReadableQ a, ReadableQ b) =>
    a -> b -> Expression (Math (TypeOf a) (TypeOf b))
(.*) left right =
    Expression (paren (getGPUCode left) <> " * " <> paren (getGPUCode right))
                Proxy
infixl 7 .*

(./) :: (HasGPUCode a, HasGPUCode b,
         ReadableQ a, ReadableQ b) =>
    a -> b -> Expression (Math (TypeOf a) (TypeOf b))
(./) left right =
    Expression (paren (getGPUCode left) <> " / " <> paren (getGPUCode right))
                Proxy
infixl 7 ./



clamp :: (HasGPUCode value, ReadableQ value, NumT (TypeOf value),
          HasGPUCode bottom, ReadableQ bottom, NumT (TypeOf bottom),
          HasGPUCode top, ReadableQ top, NumT (TypeOf top),
          TypeOf value ~ TypeOf bottom,
          TypeOf value ~ TypeOf top) =>
    value -> bottom -> top -> Expression (TypeOf value)
clamp value bottom top =
    Expression ("clamp(" <> getGPUCode value <> "," <>
                            getGPUCode bottom <> "," <>
                            getGPUCode top <> ")") Proxy

transpose :: (HasGPUCode mat, ReadableQ mat,
              TypeOf mat ~ GMat4) =>
    mat -> Expression GMat4
transpose matrix =
    Expression ("transpose(" <> getGPUCode matrix <> ")") Proxy

inverse :: (HasGPUCode mat, ReadableQ mat,
            TypeOf mat ~ GMat4) =>
    mat -> Expression GMat4
inverse matrix =
    Expression ("inverse(" <> getGPUCode matrix <> ")") Proxy

vlength :: (HasGPUCode vec, ReadableQ vec,
            VecT (TypeOf vec)) =>
    vec -> Expression GFloat
vlength vec =
    Expression ("length(" <> getGPUCode vec <> ")") Proxy


normalize :: (HasGPUCode vec, ReadableQ vec,
              VecT (TypeOf vec)) =>
    vec -> Expression (TypeOf vec)
normalize vec =
    Expression ("normalize(" <> getGPUCode vec <> ")") Proxy

reflect :: (HasGPUCode vec, ReadableQ vec,
            VecT (TypeOf vec)) =>
    vec -> vec -> Expression GFloat
reflect veca vecb =
    Expression ("reflect(" <> getGPUCode veca <> ", " <> getGPUCode vecb <> ")")
               Proxy

class IsSampler (s :: Type)
instance IsSampler GSampler2D
instance IsSampler GSampler3D

texture :: (HasGPUCode tex, ReadableQ tex,
            IsSampler (TypeOf tex),
            HasGPUCode vec, ReadableQ vec,
            VecT (TypeOf vec)) =>
    tex -> vec -> Expression GVec4
texture tex vec =
    Expression ("texture(" <> getGPUCode tex <> ", " <> getGPUCode vec <> ")")
       Proxy

glPosition :: Value 'None GVec4
glPosition = builtIn vec4 "gl_Position"

-- = For loops.

forSM :: Int -> Int -> (Value 'None GInt -> ShaderM s t ()) -> ShaderM s t ()
forSM start end action = do
    ltell . Action $ "for(int i = " <> fromString (show start) <> "; " <>
                          "i <= " <> fromString (show end) <> "; i++)\n{"
    action $ Value "i" Proxy Proxy
    ltell . Action $ "}"

forSM_ :: Int -> Int -> ShaderM s t () -> ShaderM s t ()
forSM_ start end action = do
    ltell . Action $ "for(int i = " <> fromString (show start) <> "; " <>
                          "i <= " <> fromString (show end) <> "; i++)\n{"
    action
    ltell . Action $ "}"

-- = If statements.

-- TODO: Which types are truthy?
class Truthy (a :: Type)
instance Truthy GInt
instance Truthy GUInt
instance Truthy GFloat

ifS :: (HasGPUCode bool, Truthy (TypeOf bool)) =>
    bool -> ShaderM s t () -> ShaderM s t ()
ifS condition action = do
    ltell . Action $ "if(" <> getGPUCode condition <> ") {\n"
    action
    ltell . Action $ "}"

ifElse :: (HasGPUCode bool, Truthy (TypeOf bool)) =>
    bool -> ShaderM s t () -> ShaderM s t () -> ShaderM s t ()
ifElse condition yes no = do
    ifS condition yes
    ltell . Action $ "else {\n"
    no
    ltell . Action $ "}"


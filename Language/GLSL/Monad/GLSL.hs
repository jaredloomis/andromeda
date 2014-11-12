{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language.GLSL.Monad.GLSL where

import Data.Proxy (Proxy(..))

import Control.Monad.RWS

import Data.String (fromString)
import qualified Data.ByteString as B

import Language.GLSL.Monad.Type
import Language.GLSL.Monad.GPU

---------------
-- Execution --
---------------

generateGLSL :: ShaderM s t a -> B.ByteString
generateGLSL = programBString . execShaderM

execShaderM :: ShaderM s t a -> [GLSLUnit]
execShaderM = fst . runShaderM

evalShaderM :: ShaderM s t a -> GLSLInfo t
evalShaderM = snd . runShaderM

runShaderM :: ShaderM s t a -> ([GLSLUnit], GLSLInfo t)
runShaderM shader = snd $
    execRWS (sInnerShaderM shader) () (ShaderState 0)

---------------------
-- BShow instances --
---------------------

class BShow a where
    bshow :: a -> B.ByteString

instance BShow Type where
    bshow GInt = "int"
    bshow GUInt = "uint"
    bshow GFloat = "float"
    bshow GMat4 = "mat4"
    bshow GVec2 = "vec2"
    bshow GVec3 = "vec3"
    bshow GVec4 = "vec4"
    bshow GSampler2D = "sampler2D"
    bshow GSampler3D = "sampler3D"
    bshow GPair{} = error "GLSL.hs: cannot show Pair."
    bshow GNoType = ""

instance BShow Qualifier where
    bshow In = "in"
    bshow Out = "out"
    bshow Uniform = "uniform"
    bshow None = B.empty

instance BShow Layout where
    bshow (Layout layouts)
        | not . null $ layouts =
            "layout" <> paren (B.intercalate ", " layouts)
            <> " "
        | otherwise = B.empty

instance BShow GLSLUnit where
    bshow (Version v) = "#version " <> v <> "\n"
    bshow (Decl layout None glType name) =
        bshow layout <>
        bshow glType <> " " <> name <>
        " = " <> defaultValue glType <>
        ";\n"
    bshow (Decl layout qualifier glType name) =
        bshow layout <>
        bshow qualifier <> " " <>
        bshow glType <> " " <>
        name <> ";\n"
    bshow (AssignStatement a b) =
        a <> " = " <> b <> ";\n"
    bshow (Action a) = a -- <> ";\n"

defaultValue :: Type -> B.ByteString
defaultValue GInt = "0"
defaultValue GFloat = "0.0"
defaultValue GMat4 = "mat4(1.0)"
defaultValue GVec3 = "vec3(0.0, 0.0, 0.0)"
defaultValue GVec2 = "vec2(0.0, 0.0)"
defaultValue GUInt = "0"
defaultValue GVec4 = "vec4(0.0, 0.0, 0.0, 0.0)"
defaultValue GSampler2D =
    error $ "ShaderM.defaultValue: Sampler2D does" ++
            "not have a default value."
defaultValue GSampler3D =
    error $ "ShaderM.defaultValue: Sampler3D does" ++
            "not have a default value."
defaultValue GPair{} =
    error $ "ShaderM.defaultValue: Pair does" ++
            "not have a default value."
defaultValue GNoType =
    error $ "ShaderM.defaultValue: NoType does" ++
            "not have a default value."

---------------------
-- ShaderM Codegen --
---------------------

programBString :: [GLSLUnit] -> B.ByteString
programBString xs =
    let (top, bottom) = filterTop xs
        (versions, top') = filterVersion top
        top'' = expandPairs top'
    in bshow (head versions) <>
       B.concat (map bshow top'') <>
       "\nvoid main(){\n" <>
       B.concat (map bshow bottom) <>
       "}"

filterVersion :: [GLSLUnit] -> ([GLSLUnit], [GLSLUnit])
filterVersion (v@Version{} : xs) =
    let (versions, others) = filterVersion xs
    in (v : versions, others)
filterVersion (x : xs) =
    let (versions, others) = filterVersion xs
    in (versions, x : others)
filterVersion [] = ([], [])

-- | Filter declarations that appear at
--   the top of the file.
filterTop :: [GLSLUnit] -> ([GLSLUnit], [GLSLUnit])
filterTop (v@(Version{}) : xs) =
    let (top, bottom) = filterTop xs
    in (v : top, bottom)
filterTop (u@(Decl _ Uniform _ _) : xs) =
    let (top, bottom) = filterTop xs
    in (u : top, bottom)
filterTop (i@(Decl _ In _ _) : xs) =
    let (top, bottom) = filterTop xs
    in (i : top, bottom)
filterTop (o@(Decl _ Out _ _) : xs) =
    let (top, bottom) = filterTop xs
    in (o : top, bottom)
filterTop (x : xs) =
    let (top, bottom) = filterTop xs
    in (top, x : bottom)
filterTop [] = ([], [])

expandPairs :: [GLSLUnit] -> [GLSLUnit]
expandPairs (Decl l q (GPair ta tb) n : xs) =
    let (asuf, bsuf) = pairSuffix
        decla = Decl l q ta (n <> asuf)
        declb = Decl l q tb (n <> bsuf)
    in expandPairs $ decla : declb : xs
expandPairs (x : xs) = x : expandPairs xs
expandPairs [] = []

----------------
-- Assignment --
----------------

(#=) :: (HasGPUCode a, HasGPUCode b,
         WritableQ a, ReadableQ b,
         TypeOf a ~ TypeOf b) =>
    a -> b -> ShaderM s t ()
(#=) to from =
    ltell $ AssignStatement (getGPUCode to) (getGPUCode from)
infixr 1 #=

($=) :: (HasGPUCode a, HasGPUCode b,
         WritableQ a, ReadableQ b,
         TypeOf a ~ TypeOf b) =>
    ShaderM s t a -> b -> ShaderM s t a
($=) to from = do
    toVal <- to
    toVal #= from
    return toVal
infixr 1 $=

--------------------------
-- Function application --
--------------------------

call :: B.ByteString -> ShaderM a b ()
call name = ltell . Action $ name <> "();"

(.$) :: (HasGPUCode a,
         ReadableQ a) =>
    B.ByteString -> [a] -> Expression b
(.$) = apply
infixr 3 .$

(<.) :: (HasGPUCode a,
         ReadableQ a) =>
    B.ByteString -> a -> Expression b
(<.) func x = apply func [x]
infixr 3 <.

apply :: (HasGPUCode a,
          ReadableQ a) =>
    B.ByteString -> [a] -> Expression b
apply func args = Expression
    (func <> paren (B.intercalate ", " $ map getGPUCode args))
    Proxy

----------------------
-- Declaring Arrays --
----------------------

-- TODO better.

declArray :: (Reify t Type, Reify q Qualifier) =>
    [B.ByteString] -> Proxy q -> Proxy t -> B.ByteString -> Int ->
    ShaderM s gt (Array q t)
declArray layouts qualifier glType name len = do
    let fullName = name <> "[" <> fromString (show len) <> "]"
    ltell $ Decl (Layout layouts) (reify qualifier) (reify glType) fullName
    return $ Array name qualifier glType

layoutUniformArray :: (GPU t, Reify t Type) =>
    [B.ByteString] ->
    Proxy t -> B.ByteString -> Int -> (gt -> [CPU t]) ->
    ShaderM s gt (Array 'Uniform t)
layoutUniformArray layouts glType name len values = do
    logUniformArray len glType values name
    declArray layouts (Proxy :: Proxy 'Uniform) glType name len

uniformArray :: (GPU t, Reify t Type) =>
    Proxy t -> B.ByteString -> Int -> (gt -> [CPU t]) -> ShaderM s gt (Array 'Uniform t)
uniformArray glType name len values = do
    logUniformArray len glType values name
    declArray [] (Proxy :: Proxy 'Uniform) glType name len

logUniformArray :: GPU t =>
    Int ->
    Proxy t -> (gt -> [CPU t]) -> B.ByteString -> ShaderM s gt ()
logUniformArray len glType valuesFunc name =
    forM_ [0..len] $ \i -> do
        let fullName = name <> "[" <>
                fromString (show i) <> "]"
        logUniform glType (\x -> valuesFunc x !! i) fullName

declArrayNoLen :: (Reify t Type, Reify q Qualifier) =>
    [B.ByteString] -> Proxy q -> Proxy t -> B.ByteString ->
    ShaderM s gt (Array q t)
declArrayNoLen layouts qualifier glType name = do
    let fullName = name <> "[]"
    ltell $ Decl (Layout layouts) (reify qualifier) (reify glType) fullName
    return $ Array name qualifier glType

------------------------
-- Const Declarations --
------------------------

builtInArray ::
    Proxy t -> B.ByteString -> Array 'None t
builtInArray glType name = Array name Proxy glType

builtIn :: Proxy t -> B.ByteString -> Value 'None t
builtIn glType name = Value name nonep glType

ref :: Proxy t -> B.ByteString -> Value 'None t
ref = builtIn

constant :: Proxy t -> B.ByteString -> Expression t
constant glType name = Expression name glType

constNum :: (Num a, Show a) => Proxy t -> a -> Expression t
constNum glType value = Expression (fromString $ show value) glType

constInt :: Int -> Expression GInt
constInt = constant (Proxy :: Proxy GInt) . fromString . show

constFloat :: Float -> Expression GFloat
constFloat = constant (Proxy :: Proxy GFloat) . fromString . show

rawGLSL :: B.ByteString -> ShaderM s t ()
rawGLSL = ltell . Action

-----------------
-- Other utils --
-----------------

version :: B.ByteString -> ShaderM s t ()
version = ltell . Version

paren :: B.ByteString -> B.ByteString
paren s = "(" <> s <> ")"

fltd :: Float -> Float
fltd = id

intd :: Int -> Int
intd = id

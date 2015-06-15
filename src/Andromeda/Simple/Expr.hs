{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Andromeda.Simple.Expr where

import Data.List (intercalate)
import Data.Ratio (numerator, denominator)
import Control.Arrow (second)

import Andromeda.Simple.Type
import Andromeda.Simple.GLSL
import Andromeda.Simple.Var

-- | Lambda calculus with HOAS.
data Expr a where
    Var  :: V a -> Expr a
    Lit  :: Lit a -> Expr a
    Lam  :: (Expr a -> Expr b) -> Expr (a -> b)
    (:$) :: Expr (a -> b) -> Expr a -> Expr b
infixl 9 :$

instance Show (Expr a) where
    show (Var v) = "Var (" ++ show v ++ ")"
    show (Lit l) = "Lit (" ++ show l ++ ")"
    show (Lam _) = "Lam (...)"
    show (a:$ b) = "(" ++ show a ++ ") :$ (" ++ show b ++ ")"

instance GLSL (Expr a) where
    toGLSL (Var (V n _)) = n
    toGLSL (Lit li) = toGLSL li
    toGLSL (Lam f :$ x) = toGLSL $ f x
    toGLSL app@(_ :$ _) =
        let (func, args) = collectArgs app
            argsStr = argsToStr args
        in case func of
            ExprN (Lit li) ->
                case li of
                    UnOp f  -> f ++ assertArgLen args 1 (paren argsStr)
                    BinOp f -> assertArgLen args 2 $
                        paren (toGLSL (head args))
                        ++ " " ++ f ++ " " ++
                        paren (toGLSL (last args))
                    FieldAccess f -> assertArgLen args 1 $
                        paren (toGLSL (head args)) ++ "." ++ f
                    f       -> toGLSL (Lit f) ++ paren argsStr
            ExprN (Var (V n _)) -> n ++ paren argsStr
            ExprN (Lam _) -> --toGLSL (betaReduce app)
                error $
                "toGLSL Expr: recieved Lam. Expr must be" ++
                " beta reduced. Use 'betaReduce'."
            ExprN (_:$_) -> error
                "toGLSL Expr: matched on an impossible case."
      where
        assertArgLen args len x
            | length args /= len =
                error $
                    "Error in 'assertArgLen': wanted " ++ show len ++
                    ", recieved " ++ show (length args) ++ "."
            | otherwise = x
    toGLSL (Lam _) =
        error "Error in toGLSL Expr: recieved unapplied Lam."

---------------------------------------
-- Helpers for GLSL Expr instance --
---------------------------------------

-- | Existential Expr.
data ExprN = forall a. ExprN (Expr a)
instance GLSL ExprN where
    toGLSL (ExprN e) = toGLSL e

collectArgsR :: Expr a -> (ExprN, [ExprN])
collectArgsR (xs :$ x) =
    let (f,as) = collectArgsR xs
    in (f, ExprN x : as)
collectArgsR x = (ExprN x,[])

argList :: Expr a -> String
argList = argsToStr . snd . collectArgs

collectArgs :: Expr a -> (ExprN, [ExprN])
collectArgs = second reverse . collectArgsR

argsToStr :: [ExprN] -> String
argsToStr =  intercalate ", " . map toGLSL

paren :: String -> String
paren str = "(" ++ str ++ ")"

---------------------------
-- Useful Expr instances --
---------------------------

instance (GLSL a, Enum a, Num a) => Enum (Expr a) where
    toEnum = Lit . Literal . toEnum

    fromEnum (Lit (Literal x)) = fromEnum x
    fromEnum _ = error
        "fromEnum Expr failed. fromEnum only works on 'Lit (Literal x)'"

    succ = (+1)
    pred = (+(-1))

    enumFrom     s   = s : enumFrom (succ s)
    enumFromThen s d = s : enumFromThen (s+d) d

instance (Num a, GLSL a) => Num (Expr a) where
    fromInteger int = Lit $ Literal (fromInteger int :: a)
    a + b  = Lit (BinOp "+") :$ a :$ b
    a - b  = Lit (BinOp "-") :$ a :$ b
    a * b  = Lit (BinOp "*") :$ a :$ b
    abs x  = Lit (Native "abs") :$ x
    signum = error "Called signum on 'Expr a'."

instance (Fractional a, GLSL a) => Fractional (Expr a) where
    fromRational rat = Lit (BinOp "/") :$
                       (fromInteger (numerator rat) :: Expr Int) :$
                       (fromInteger (denominator rat) :: Expr Int)
    n / d = Lit (BinOp "/") :$ n :$ d

instance (Floating a, GLSL a) => Floating (Expr a) where
    pi      = Lit (Literal pi)
    exp   x = Lit (Native "exp")   :$ x
    log   x = Lit (Native "log")   :$ x
    sin   x = Lit (Native "sin")   :$ x
    cos   x = Lit (Native "cos")   :$ x
    tan   x = Lit (Native "tan")   :$ x
    asin  x = Lit (Native "asin")  :$ x
    acos  x = Lit (Native "acos")  :$ x
    atan  x = Lit (Native "atan")  :$ x
    sinh  x = Lit (Native "sinh")  :$ x
    cosh  x = Lit (Native "cosh")  :$ x
    tanh  x = Lit (Native "tanh")  :$ x
    asinh x = Lit (Native "asinh") :$ x
    acosh x = Lit (Native "acosh") :$ x
    atanh x = Lit (Native "atanh") :$ x

-------------
-- Literal --
-------------

-- | A GLSL Literal. More constructors should only
--   be added for glsl operations with special syntax.
data Lit a where
    Literal :: GLSL a => a -> Lit a
    Native  :: String -> Lit a

    BinOp   :: String -> Lit a
    UnOp    :: String -> Lit a

    FieldAccess :: String -> Lit a

    Pair    :: Lit (a -> b -> (a, b))

--    LitIn   :: HasVertex a => String -> Type a -> [a] -> Lit a
--    LitUnif :: HasVertex a => String -> Type a ->  a  -> Lit a

    Fetch   :: String -> Type a -> Lit a
    Unif    :: String -> Type a -> Lit a

instance Show (Lit a) where
    show (Literal l) = "Literal (" ++ toGLSL l ++ ")"
    show (Native  n) = "Native (" ++ n ++ ")"
    show (BinOp b) = "BinOp (" ++ b ++ ")"
    show (UnOp u) = "UnOp (" ++ u ++ ")"
    show (FieldAccess f) = "FieldAccess " ++ f
    show Pair = "Pair"
--    show LitIn{} = "LitIn"
--    show LitUnif{} = "LitUnif"
    show (Fetch n _) = "Fetch " ++ show n
    show (Unif  n _) = "Unif "  ++ show n

instance GLSL (Lit a) where
    toGLSL (Literal a) = toGLSL a
    toGLSL (Native s) = s
    toGLSL (BinOp s) = s
    toGLSL (UnOp s) = s
    toGLSL (FieldAccess f) = f
    toGLSL Pair = error "toGLSL Pair"
--    toGLSL LitIn{} = error "toGLSL LitIn"
--    toGLSL LitUnif{} = error "toGLSL LitUnif"
    toGLSL (Fetch n _) = n
    toGLSL (Unif  n _) = n

-------------
-- Pattern --
-------------

pairPat :: String -> (String, String)
pairPat name = (name ++ "_pairA", name ++ "pairB")

-----------------
-- Beta reduce --
-----------------

-- | Beta reduce an 'Expr' by applying all
--   'Lam's, where possible. All Exprs should
--   be beta-reduced before being used.
betaReduce :: Expr a -> Expr a
betaReduce (Lam f :$ x) = betaReduce $ f (betaReduce x)
betaReduce (f :$ x)     = betaReduce' $ betaReduce f :$ betaReduce x
  where
    betaReduce' (Lam f' :$ x') = betaReduce $ f' (betaReduce x')
    betaReduce' (f'     :$ x') = betaReduce f' :$ betaReduce x'
    betaReduce' x'             = x'
betaReduce v@Var{} = v
betaReduce l@Lit{} = l
betaReduce l@Lam{} = l

------------
-- unPair --
------------

unPair :: forall a b. (Typed a, Typed b) =>
    Expr (a, b) -> (Expr a, Expr b)
unPair = unPair' . betaReduce

unPair' :: forall a b. (Typed a, Typed b) =>
    Expr (a, b) -> (Expr a, Expr b)
unPair' (Lit Pair :$ a :$ b) = (a, b)
unPair' (Var (V name (tyA :*: tyB))) =
    let (varA, varB) = pairPat name
    in (Var (V varA tyA), Var (V varB tyB))
unPair' expr = error $
    "unPair: Pair was made through some " ++
    "unknown operation:\n" ++ show expr

-------------
-- typeOfE --
-------------

typeOfE :: Typed a => Expr a -> Type a
typeOfE _ = guessTy

module Main where

import Test.QuickCheck

import Andromeda.Simple.StdLib

main :: IO ()
main = quickCheck test

test :: [Int] -> Bool
test xs = reverse (reverse xs) == xs

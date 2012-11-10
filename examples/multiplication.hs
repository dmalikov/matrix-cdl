#!/usr/bin/runhaskell
{-# LANGUAGE UnicodeSyntax #-}
import Data.Array.Repa
import Data.Array.Repa.Algorithms.Matrix.Boolean

examplePassed :: Bool
examplePassed = x `mmultSL` b == a
  where
    a = fromListUnboxed (Z :. (2::Int) :. (3::Int)) [False,True,False,True, True, True]
    b = fromListUnboxed (Z :. (2::Int) :. (3::Int)) [True,False,True,False,True,False]
    x = fromListUnboxed (Z :. (2::Int) :. (2::Int)) [False, True, True, True]

main ∷ IO ()
main = print examplePassed

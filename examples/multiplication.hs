#!/usr/bin/runhaskell
{-# LANGUAGE UnicodeSyntax #-}

import Data.Array.Boolean

examplePassed :: Bool
examplePassed = x `mult` b == a
  where
    a = Matrix (2,3) [False,True,False,True,True,True]
    b = Matrix (2,3) [True,False,True,False,True,False]
    x = Matrix (2,2) [False,True,True,True]

main ∷ IO ()
main = print examplePassed

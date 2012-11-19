#!/usr/bin/runhaskell
{-# LANGUAGE UnicodeSyntax #-}

import Data.Array.Boolean

examplePassed ∷ Bool
examplePassed = a `divisible` b
  where
    a = Matrix (2,3) [False, False, True, True, True, True]
    b = Matrix (4,3) [False, True, False, False, False, False, False, False, True, True, False, False]

main ∷ IO ()
main = print examplePassed

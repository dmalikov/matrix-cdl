{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE Rank2Types #-}

module Data.Array.Repa.Algorithms.Matrix.Boolean
  ( mmultSL, transpose2SL
  ) where

import Data.Algebra.Boolean

import Data.Array.Repa as R
import Data.Array.Repa.Eval as R
import Data.Array.Repa.Unsafe as R
import Data.Array.Repa.Algorithms.Matrix
import Data.Array.Repa.Operators.Reduction.Boolean

import Data.Vector.Unboxed as V hiding (null)
import Control.Monad.ST.Strict

{-
-- MmultL ----------------------------------------------------------------------
-- | Matrix matrix multiply, in parallel.
mmultPL ∷ (CDL l, Elt l, V.Unbox l, Monad m) ⇒ Matrix l → Matrix l → m (Matrix l)
mmultPL arr brr = [arr, brr] `deepSeqArrays` do
  trr ← transpose2PL brr
  let (Z :. h1  :. _)  = extent arr
  let (Z :. _   :. w2) = extent brr
  computeP $ fromFunction (Z :. h1 :. w2) $ \ix → joinAllS $ R.zipWith meet
    (R.unsafeSlice arr (Any :. row ix :. All))
    (R.unsafeSlice trr (Any :. col ix :. All))
{-# NOINLINE mmultPL #-}
-}

-- | Matrix matrix multiply, sequentially.
mmultSL ∷ (Elt l, V.Unbox l) ⇒ Matrix l → Matrix l → Matrix l
mmultSL arr brr = [arr, brr] `deepSeqArrays` runST $ do
  trr ← R.now $ transpose2SL brr
  let (Z :. h1  :. _)  = extent arr
  let (Z :. _   :. w2) = extent brr
  return $ computeS $ fromFunction (Z :. h1 :. w2) $ \ix → joinAllS $ R.zipWith meet
    (R.unsafeSlice arr (Any :. row ix :. All))
    (R.unsafeSlice trr (Any :. col ix :. All))
{-# NOINLINE mmultSL #-}

{-
-- TransposeL ------------------------------------------------------------------
-- | Transpose a 2D matrix, in parallel.
transpose2PL ∷ Monad m ⇒ Matrix l → m (Matrix l)
transpose2PL arr = arr `deepSeqArray` computeUnboxedP $
  R.unsafeBackpermute new_extent swap arr
 where
  swap (Z :. i :. j) = Z :. j :. i
  new_extent         = swap (extent arr)
{-# NOINLINE transpose2PL #-}
-}


-- | Transpose a 2D matrix, sequentially.
transpose2SL ∷ V.Unbox l ⇒ Matrix l → Matrix l
transpose2SL arr = arr `deepSeqArray` computeUnboxedS $
  R.unsafeBackpermute new_extent swap arr
 where
  swap (Z :. i :. j) = Z :. j :. i
  new_extent         = swap (extent arr)
{-# NOINLINE transpose2SL #-}

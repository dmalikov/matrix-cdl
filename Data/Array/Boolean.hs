{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE Rank2Types #-}

module Data.Array.Boolean
  ( Matrix(..), Bounds, (!*), row, col, mult
  , findWhich, rows, cols, divisible
  ) where

import Data.Algebra.Boolean

type Bounds = (Int, Int)
data Matrix α = Matrix Bounds [α]
  deriving (Eq, Read, Show)

bds ∷ Matrix α → Bounds
bds (Matrix b _ ) = b

els ∷ Matrix α → [α]
els (Matrix _ e) = e

infix 5 !*
( !* ) ∷ Matrix α → Bounds → α
m !* (i,j) = let (x_i,y_i) = bds m in
  if (i > x_i) || (j > y_i)
    then error "No such element"
    else els m !! ((i-1)*y_i + (j-1))

row ∷ Matrix α → Int → Matrix α
row m r = let (xi,xj) = bds m in
  if r <= xi && r > 0
    then Matrix (1,xj) [ m !* (r,j) | j <- [1..xj] ]
    else error "no such row"

col ∷ Matrix α → Int → Matrix α
col m c = let (xi,xj) = bds m in
  if c <= xj && c > 0
    then Matrix (xi,1) [ m !* (i,c) | i <- [1..xi] ]
    else error "no such col"

rows ∷ Matrix α → [Matrix α]
rows m = row m `map` [1 .. fst $ bds m ]

cols ∷ Matrix α → [Matrix α]
cols m = col m `map` [1 .. snd $ bds m ]

findWhich ∷ (α → Bool) → Matrix α → [Bounds]
findWhich p m = let (xi,xj) = bds m in
  [ (i,j) | i ← [1..xi], j ← [1..xj], p (m !* (i,j)) ]

mult ∷ CDL α ⇒ Matrix α → Matrix α → Matrix α
mult x y = Matrix newBounds
  [ foldr1 join [ meet (x !* (i,k)) (y !* (k,j)) | k <- [1..xj]] | i <- [1..xi], j <- [1..yj] ]
    where
      (xi,xj) = bds x
      (yi,yj) = bds y
      newBounds | xj == yi = (xi,yj)
                | otherwise = error "mult: wrong dimensions"

divisible ∷ Matrix Bool → Matrix Bool → Bool
divisible a b = and . map f . rows $ a
  where
    f a' = (== els a') $
      foldl1 (zipWith (||)) $
      map els $
      foldl (\l k -> filter (\x -> x !* k == False) l) (rows b) $
      findWhich (== False) a'

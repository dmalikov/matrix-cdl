{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE Rank2Types #-}
module Data.Algebra.Boolean
  ( CDL(..)
  ) where


-- | CDL stands for «complement distributive lattice»

class CDL a where
  meet ∷ a → a → a
  join ∷ a → a → a
  complement ∷ a → a
  null ∷ a
  id ∷ a

instance CDL Bool where
  meet = (&&)
  join = (||)
  complement = not
  null = False
  id = True

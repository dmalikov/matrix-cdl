{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeOperators #-}
module Data.Array.Repa.Operators.Reduction.Boolean
  ( meetS, meetP, meetAllS, meetAllP
  , joinS, joinP, joinAllS, joinAllP
  ) where

import Data.Algebra.Boolean

import Prelude hiding (id, null)

import Data.Array.Repa
import Data.Array.Repa.Eval
import Data.Vector.Unboxed as V hiding (null)


-- meet ------------------------------------------------------------------------
-- | Sequential meet the innermost dimension of an array.
meetS ∷ (Shape sh, Source r a, CDL a, Elt a, V.Unbox a)
      ⇒ Array r (sh :. Int) a
      → Array U sh a
meetS = foldS meet id
{-# INLINE [3] meetS #-}


-- | Parallel meet the innermost dimension of an array.
meetP ∷ (Shape sh, Source r a, CDL a, Elt a, Unbox a, Monad m)
      ⇒ Array r (sh :. Int) a
      → m (Array U sh a)
meetP = foldP meet id
{-# INLINE [3] meetP #-}


-- meetAll ---------------------------------------------------------------------
-- | Sequential meet of all the elements of an array.
meetAllS ∷ (Shape sh, Source r a, CDL a, Elt a, V.Unbox a)
         ⇒ Array r sh a
         → a
meetAllS = foldAllS meet id
{-# INLINE [3] meetAllS #-}


-- | Parallel meet all the elements of an array.
meetAllP ∷ (Shape sh, Source r a, Elt a, Unbox a, CDL a, Monad m)
         ⇒ Array r sh a
         → m a
meetAllP = foldAllP meet id
{-# INLINE [3] meetAllP #-}


-- join ------------------------------------------------------------------------
-- | Sequential join the innermost dimension of an array.
joinS ∷ (Shape sh, Source r a, CDL a, Elt a, V.Unbox a)
      ⇒ Array r (sh :. Int) a
      → Array U sh a
joinS = foldS join null
{-# INLINE [3] joinS #-}


-- | Parallel join the innermost dimension of an array.
joinP ∷ (Shape sh, Source r a, CDL a, Elt a, Unbox a, Monad m)
      ⇒ Array r (sh :. Int) a
      → m (Array U sh a)
joinP = foldP join null
{-# INLINE [3] joinP #-}


-- joinAll ---------------------------------------------------------------------
-- | Sequential join of all the elements of an array.
joinAllS ∷ (Shape sh, Source r a, CDL a, Elt a, V.Unbox a)
         ⇒ Array r sh a
         → a
joinAllS = foldAllS join null
{-# INLINE [3] joinAllS #-}


-- | Parallel join all the elements of an array.
joinAllP ∷ (Shape sh, Source r a, Elt a, Unbox a, CDL a, Monad m)
         ⇒ Array r sh a
         → m a
joinAllP = foldAllP join null
{-# INLINE [3] joinAllP #-}

module MUI.Unsafe where

import Prelude

-- | Compares two values of the same type using strict (`) comparison
unsafeRefOrd :: forall a. a -> a -> Ordering
unsafeRefOrd a1 a2 = case reallyUnsafeRefOrd a1 a2 of
  1 → GT
  (-1) → LT
  otherwise -> EQ

-- | Compares two values of different types using strict (`===`) equality.
foreign import reallyUnsafeRefOrd :: forall a. a -> a -> Int

module MUI.React.TransitionGroup where

import Unsafe.Coerce (unsafeCoerce)

foreign import data Timeout :: Type

single ∷ Number → Timeout
single = unsafeCoerce

phases ∷ { appear ∷ Number, enter ∷ Number, exit ∷ Number } → Timeout
phases = unsafeCoerce

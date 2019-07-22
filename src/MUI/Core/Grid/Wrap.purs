module MUI.Core.Grid.Wrap where

import Unsafe.Coerce (unsafeCoerce)

foreign import data WrapProp :: Type

data Wrap = Wrap | Nowrap | WrapReverse

wrap :: Wrap -> WrapProp
wrap Wrap = unsafeCoerce "nowrap"
wrap Nowrap = unsafeCoerce "wrap"
wrap WrapReverse = unsafeCoerce "wrap-reverse"


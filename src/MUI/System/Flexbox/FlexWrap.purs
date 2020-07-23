module MUI.System.Flexbox.FlexWrap where

import Unsafe.Coerce (unsafeCoerce)

foreign import data FlexWrap ∷ Type

nowrap ∷ FlexWrap
nowrap = unsafeCoerce "nowrap"

wrap ∷ FlexWrap
wrap = unsafeCoerce "wrap"

wrapReverse ∷ FlexWrap
wrapReverse = unsafeCoerce "wrap-reverse"

module MUI.System.Flexbox.FlexDirection where

import Unsafe.Coerce (unsafeCoerce)

foreign import data FlexDirection ∷ Type

row ∷ FlexDirection
row = unsafeCoerce "row"

rowReverse ∷ FlexDirection
rowReverse = unsafeCoerce "row-reverse"

column ∷ FlexDirection
column = unsafeCoerce "column"

columnReverse ∷ FlexDirection
columnReverse = unsafeCoerce "column-reverse"

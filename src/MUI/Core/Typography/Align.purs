module MUI.Core.Typography.Align where

import Unsafe.Coerce (unsafeCoerce)

foreign import data AlignProp :: Type

data Align = Inherit | Left | Center | Right | Justify 

align :: Align -> AlignProp
align Inherit = unsafeCoerce "inherit"
align Left = unsafeCoerce "left"
align Center = unsafeCoerce "center"
align Right = unsafeCoerce "right"
align Justify = unsafeCoerce "justify"
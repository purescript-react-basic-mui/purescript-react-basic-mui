module MUI.System.Sizing where

import Unsafe.Coerce (unsafeCoerce)

foreign import data BoxSizing ∷ Type

contentBox :: BoxSizing
contentBox = unsafeCoerce "content-box"

borderBox :: BoxSizing
borderBox = unsafeCoerce "border-box"

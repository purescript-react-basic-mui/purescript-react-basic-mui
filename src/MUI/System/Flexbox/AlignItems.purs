module MUI.System.Flexbox.AlignItems where

import Unsafe.Coerce (unsafeCoerce)

foreign import data AlignItems :: Type

stretch :: AlignItems
stretch = unsafeCoerce "stretch"

center :: AlignItems
center = unsafeCoerce "center"

start :: AlignItems
start = unsafeCoerce "start"

end :: AlignItems
end = unsafeCoerce "end"

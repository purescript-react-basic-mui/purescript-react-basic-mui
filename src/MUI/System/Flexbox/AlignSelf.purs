module MUI.System.Flexbox.AlignSelf where

import Unsafe.Coerce (unsafeCoerce)

foreign import data AlignSelf :: Type

stretch :: AlignSelf
stretch = unsafeCoerce "stretch"

center :: AlignSelf
center = unsafeCoerce "center"

start :: AlignSelf
start = unsafeCoerce "start"

end :: AlignSelf
end = unsafeCoerce "end"

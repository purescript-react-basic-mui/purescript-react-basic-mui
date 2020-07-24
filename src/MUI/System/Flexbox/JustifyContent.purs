module MUI.System.Flexbox.JustifyContent where

import Unsafe.Coerce (unsafeCoerce)

foreign import data JustifyContent :: Type

flexStart :: JustifyContent
flexStart = unsafeCoerce "flex-start"

flexEnd :: JustifyContent
flexEnd = unsafeCoerce "flex-end"

center :: JustifyContent
center = unsafeCoerce "center"

spaceBetween :: JustifyContent
spaceBetween = unsafeCoerce "space-between"

spaceAround :: JustifyContent
spaceAround = unsafeCoerce "space-around"

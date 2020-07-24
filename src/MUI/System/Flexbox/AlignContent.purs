module MUI.System.Flexbox.AlignContent where

import Unsafe.Coerce (unsafeCoerce)

foreign import data AlignContent :: Type

start :: AlignContent
start = unsafeCoerce "start"

center :: AlignContent
center = unsafeCoerce "center"

spaceBetween :: AlignContent
spaceBetween = unsafeCoerce "space-between"

spaceAround :: AlignContent
spaceAround = unsafeCoerce "space-around"

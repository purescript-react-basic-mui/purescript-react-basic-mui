module MUI.Core.Grid.AlignContent where

import Prelude

import Unsafe.Coerce (unsafeCoerce)

foreign import data AlignContentProp :: Type
foreign import _eqAlignContentProp :: AlignContentProp -> AlignContentProp -> Boolean
foreign import _ordAlignContentProp :: AlignContentProp -> AlignContentProp -> Int
instance eqAlignContentProp :: Eq AlignContentProp where eq left right = _eqAlignContentProp left right
instance ordAlignContentProp :: Ord AlignContentProp where compare left right = compare (_ordAlignContentProp left right) (_ordAlignContentProp right left)

stretch :: AlignContentProp
stretch = unsafeCoerce "stretch"

center :: AlignContentProp
center = unsafeCoerce "center"

flexStart :: AlignContentProp
flexStart = unsafeCoerce "flex-start"

flexEnd :: AlignContentProp
flexEnd = unsafeCoerce "flex-end"

spaceBetween :: AlignContentProp
spaceBetween = unsafeCoerce "space-between"

spaceAround :: AlignContentProp
spaceAround = unsafeCoerce "space-around"
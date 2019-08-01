module MUI.Core.Grid.AlignContent where

import Prelude

import Unsafe.Coerce (unsafeCoerce)

foreign import data AlignContentProp :: Type
foreign import _eqAlignContentProp :: AlignContentProp -> AlignContentProp -> Boolean
foreign import _ordAlignContentProp :: AlignContentProp -> AlignContentProp -> Int
instance eqAlignContentProp :: Eq AlignContentProp where eq _left _right = _eqAlignContentProp _left _right
instance ordAlignContentProp :: Ord AlignContentProp where compare _left _right = compare (_ordAlignContentProp _left _right) (_ordAlignContentProp _right _left)

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
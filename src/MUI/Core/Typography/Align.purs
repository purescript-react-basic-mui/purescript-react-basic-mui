module MUI.Core.Typography.Align where

import Prelude

import Unsafe.Coerce (unsafeCoerce)

foreign import data AlignProp :: Type
foreign import _eqAlignProp :: AlignProp -> AlignProp -> Boolean
foreign import _ordAlignProp :: AlignProp -> AlignProp -> Int
instance eqAlignProp :: Eq AlignProp where eq _left _right = _eqAlignProp _left _right
instance ordAlignProp :: Ord AlignProp where compare _left _right = compare (_ordAlignProp _left _right) (_ordAlignProp _right _left)

inherit :: AlignProp
inherit = unsafeCoerce "inherit"

left :: AlignProp
left = unsafeCoerce "left"

center :: AlignProp
center = unsafeCoerce "center"

right :: AlignProp
right = unsafeCoerce "right"

justify :: AlignProp
justify = unsafeCoerce "justify"
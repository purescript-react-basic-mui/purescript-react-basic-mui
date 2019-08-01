module MUI.Core.Grid.AlignItems where

import Prelude

import Unsafe.Coerce (unsafeCoerce)

foreign import data AlignItemsProp :: Type
foreign import _eqAlignItemsProp :: AlignItemsProp -> AlignItemsProp -> Boolean
foreign import _ordAlignItemsProp :: AlignItemsProp -> AlignItemsProp -> Int
instance eqAlignItemsProp :: Eq AlignItemsProp where eq _left _right = _eqAlignItemsProp _left _right
instance ordAlignItemsProp :: Ord AlignItemsProp where compare _left _right = compare (_ordAlignItemsProp _left _right) (_ordAlignItemsProp _right _left)

flexStart :: AlignItemsProp
flexStart = unsafeCoerce "flex-start"

center :: AlignItemsProp
center = unsafeCoerce "center"

flexEnd :: AlignItemsProp
flexEnd = unsafeCoerce "flex-end"

stretch :: AlignItemsProp
stretch = unsafeCoerce "stretch"

baseline :: AlignItemsProp
baseline = unsafeCoerce "baseline"
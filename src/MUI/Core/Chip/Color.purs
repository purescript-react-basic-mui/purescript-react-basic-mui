module MUI.Core.Chip.Color where

import Prelude

import Unsafe.Coerce (unsafeCoerce)

foreign import data ColorProp :: Type
foreign import _eqColorProp :: ColorProp -> ColorProp -> Boolean
foreign import _ordColorProp :: ColorProp -> ColorProp -> Int
instance eqColorProp :: Eq ColorProp where eq _left _right = _eqColorProp _left _right
instance ordColorProp :: Ord ColorProp where compare _left _right = compare (_ordColorProp _left _right) (_ordColorProp _right _left)

primary :: ColorProp
primary = unsafeCoerce "primary"

secondary :: ColorProp
secondary = unsafeCoerce "secondary"

default :: ColorProp
default = unsafeCoerce "default"
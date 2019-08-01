module MUI.Core.Typography.Color where

import Prelude

import Unsafe.Coerce (unsafeCoerce)

foreign import data ColorProp :: Type
foreign import _eqColorProp :: ColorProp -> ColorProp -> Boolean
foreign import _ordColorProp :: ColorProp -> ColorProp -> Int
instance eqColorProp :: Eq ColorProp where eq _left _right = _eqColorProp _left _right
instance ordColorProp :: Ord ColorProp where compare _left _right = compare (_ordColorProp _left _right) (_ordColorProp _right _left)

initial :: ColorProp
initial = unsafeCoerce "initial"

inherit :: ColorProp
inherit = unsafeCoerce "inherit"

primary :: ColorProp
primary = unsafeCoerce "primary"

secondary :: ColorProp
secondary = unsafeCoerce "secondary"

textPrimary :: ColorProp
textPrimary = unsafeCoerce "textPrimary"

textSecondary :: ColorProp
textSecondary = unsafeCoerce "textSecondary"

error :: ColorProp
error = unsafeCoerce "error"
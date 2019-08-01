module MUI.Core.Typography.Display where

import Prelude

import Unsafe.Coerce (unsafeCoerce)

foreign import data DisplayProp :: Type
foreign import _eqDisplayProp :: DisplayProp -> DisplayProp -> Boolean
foreign import _ordDisplayProp :: DisplayProp -> DisplayProp -> Int
instance eqDisplayProp :: Eq DisplayProp where eq _left _right = _eqDisplayProp _left _right
instance ordDisplayProp :: Ord DisplayProp where compare _left _right = compare (_ordDisplayProp _left _right) (_ordDisplayProp _right _left)

initial :: DisplayProp
initial = unsafeCoerce "initial"

block :: DisplayProp
block = unsafeCoerce "block"

inline :: DisplayProp
inline = unsafeCoerce "inline"
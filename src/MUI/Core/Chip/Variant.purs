module MUI.Core.Chip.Variant where

import Prelude

import Unsafe.Coerce (unsafeCoerce)

foreign import data VariantProp :: Type
foreign import _eqVariantProp :: VariantProp -> VariantProp -> Boolean
foreign import _ordVariantProp :: VariantProp -> VariantProp -> Int
instance eqVariantProp :: Eq VariantProp where eq _left _right = _eqVariantProp _left _right
instance ordVariantProp :: Ord VariantProp where compare _left _right = compare (_ordVariantProp _left _right) (_ordVariantProp _right _left)

default :: VariantProp
default = unsafeCoerce "default"

outlined :: VariantProp
outlined = unsafeCoerce "outlined"
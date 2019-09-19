module MUI.Core.Chip.SizeProp where

import Prelude

import Unsafe.Coerce (unsafeCoerce)

foreign import data SizeProp :: Type
foreign import _eqSizeProp :: SizeProp -> SizeProp -> Boolean
foreign import _ordSizeProp :: SizeProp -> SizeProp -> Int
instance eqSizeProp :: Eq SizeProp where eq _left _right = _eqSizeProp _left _right
instance ordSizeProp :: Ord SizeProp where compare _left _right = compare (_ordSizeProp _left _right) (_ordSizeProp _right _left)

small :: SizeProp
small = unsafeCoerce "small"

medium :: SizeProp
medium = unsafeCoerce "medium"
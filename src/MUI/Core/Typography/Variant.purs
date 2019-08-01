module MUI.Core.Typography.Variant where

import Prelude

import Unsafe.Coerce (unsafeCoerce)

foreign import data VariantProp :: Type
foreign import _eqVariantProp :: VariantProp -> VariantProp -> Boolean
foreign import _ordVariantProp :: VariantProp -> VariantProp -> Int
instance eqVariantProp :: Eq VariantProp where eq _left _right = _eqVariantProp _left _right
instance ordVariantProp :: Ord VariantProp where compare _left _right = compare (_ordVariantProp _left _right) (_ordVariantProp _right _left)

h1 :: VariantProp
h1 = unsafeCoerce "h1"

h2 :: VariantProp
h2 = unsafeCoerce "h2"

h3 :: VariantProp
h3 = unsafeCoerce "h3"

h4 :: VariantProp
h4 = unsafeCoerce "h4"

h5 :: VariantProp
h5 = unsafeCoerce "h5"

h6 :: VariantProp
h6 = unsafeCoerce "h6"

subtitle1 :: VariantProp
subtitle1 = unsafeCoerce "subtitle1"

subtitle2 :: VariantProp
subtitle2 = unsafeCoerce "subtitle2"

body1 :: VariantProp
body1 = unsafeCoerce "body1"

body2 :: VariantProp
body2 = unsafeCoerce "body2"

caption :: VariantProp
caption = unsafeCoerce "caption"

button :: VariantProp
button = unsafeCoerce "button"

overline :: VariantProp
overline = unsafeCoerce "overline"

srOnly :: VariantProp
srOnly = unsafeCoerce "srOnly"

inherit :: VariantProp
inherit = unsafeCoerce "inherit"
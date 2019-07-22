module MUI.Core.Typography.Variant where

import Unsafe.Coerce (unsafeCoerce)

foreign import data VariantProp :: Type

data Variant
  = H1
  | H2
  | H3
  | H4
  | H5
  | H6
  | Subtitle1
  | Subtitle2
  | Body1
  | Body2
  | Caption
  | Button
  | Overline
  | SrOnly
  | Inherit

variant :: Variant -> VariantProp
variant H1 = unsafeCoerce "h1"
variant H2 = unsafeCoerce "h2"
variant H3 = unsafeCoerce "h3"
variant H4 = unsafeCoerce "h4"
variant H5 = unsafeCoerce "h5"
variant H6 = unsafeCoerce "h6"
variant Subtitle1 = unsafeCoerce "subtitle1"
variant Subtitle2 = unsafeCoerce "subtitle2"
variant Body1 = unsafeCoerce "body1"
variant Body2 = unsafeCoerce "body2"
variant Caption = unsafeCoerce "caption"
variant Button = unsafeCoerce "button"
variant Overline = unsafeCoerce "overline"
variant SrOnly = unsafeCoerce "srOnly"
variant Inherit = unsafeCoerce "inherit"
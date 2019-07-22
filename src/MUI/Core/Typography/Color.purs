module MUI.Core.Typography.Color where

import Unsafe.Coerce (unsafeCoerce)

foreign import data ColorProp :: Type
data Color = Initial | Default | Error | Inherit | Primary | Secondary | TextPrimary | TextSecondary
color :: Color -> ColorProp
color Initial = unsafeCoerce "initial"
color Default = unsafeCoerce "default"
color Error = unsafeCoerce "error"
color Inherit = unsafeCoerce "inherit"
color Primary = unsafeCoerce "primary"
color Secondary = unsafeCoerce "secondary"
color TextPrimary = unsafeCoerce "textPrimary"
color TextSecondary = unsafeCoerce "textSecondary"
module MUI.Core.Icon.Color where

import Unsafe.Coerce (unsafeCoerce)

foreign import data ColorProp :: Type
data Color = Inherit | Primary | Secondary | Action | Error | Disabled

color :: Color -> ColorProp
color Inherit = unsafeCoerce "inherit"
color Primary = unsafeCoerce "primary"
color Secondary = unsafeCoerce "secondary"
color Action = unsafeCoerce "action"
color Error = unsafeCoerce "error"
color Disabled = unsafeCoerce "disabled"

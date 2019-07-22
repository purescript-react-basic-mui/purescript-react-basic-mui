module MUI.Core.Icon.FontSize where

import Unsafe.Coerce (unsafeCoerce)

foreign import data FontSizeProp :: Type

data FontSize = Inherit | Default | Small | Large

fontSize :: FontSize -> FontSizeProp
fontSize Inherit = unsafeCoerce "inherit"
fontSize Default = unsafeCoerce "default"
fontSize Small = unsafeCoerce "small"
fontSize Large = unsafeCoerce "large"
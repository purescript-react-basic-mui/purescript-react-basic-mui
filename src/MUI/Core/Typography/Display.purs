module MUI.Core.Typography.Display where

import Unsafe.Coerce (unsafeCoerce)

foreign import data DisplayProp :: Type

data Display = Initial | Block | Inline
display :: Display -> DisplayProp
display Initial = unsafeCoerce "initial"
display Block = unsafeCoerce "block"
display Inline = unsafeCoerce "inline"



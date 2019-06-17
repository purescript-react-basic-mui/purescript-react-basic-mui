module React.Basic.MUI.Core.Button where 

import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic.MUI.Core.ButtonBase (ExtendButtonBase, ExtendButtonBaseTypeMap)
import React.Basic.MUI.Core.OverridableComponent (OverrideProps)

type ButtonTypeMap p d = Foreign

button :: ExtendButtonBase ButtonTypeMap {  } String
button = _Button
foreign import _Button :: ExtendButtonBase ButtonTypeMap {  } String

type ButtonProps d p = OverrideProps ButtonTypeMap Foreign Foreign Foreign

type ButtonClassKey = Foreign
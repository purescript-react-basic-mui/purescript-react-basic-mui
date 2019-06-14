module React.Basic.MUI.Button where 

import Prelude
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic.MUI.ButtonBase (ExtendButtonBase)

type ButtonTypeMap p d = ReactComponent

button :: ExtendButtonBase ButtonTypeMap {  } String
button = _Button
foreign import _Button :: ExtendButtonBase ButtonTypeMap {  } String

type ButtonProps d p = OverrideProps ButtonTypeMap Foreign Foreign Foreign

type ButtonClassKey = Foreign
module React.Basic.MUI.Core.ButtonBase where 

import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic.Events (EventHandler)
import React.Basic.MUI.Core.ButtonBase.TouchRipple (TouchRippleProps)
import React.Basic.MUI.Core.OverridableComponent (SimplifiedPropsOf, OverrideProps)

type ButtonBaseTypeMap_required  optional =
  ( props :: { action :: Foreign, buttonRef :: Foreign, centerRipple :: Boolean, disabled :: Boolean, disableRipple :: Boolean, disableTouchRipple :: Boolean, focusRipple :: Boolean, focusVisibleClassName :: String, onFocusVisible :: EventHandler, "TouchRippleProps" :: Foreign }
  , defaultComponent :: String
  , classKey :: Foreign
  | optional )

type ButtonBaseTypeMap_optional =
  ( 
  )

foreign import data ButtonBaseTypeMap :: Type 



type ExtendButtonBaseTypeMap_required  m optional =
  ( props :: Foreign
  , defaultComponent :: Foreign
  , classKey :: Foreign
  | optional )

type ExtendButtonBaseTypeMap_optional m =
  ( 
  )

foreign import data ExtendButtonBaseTypeMap :: Type 



type ExtendButtonBase m = Foreign

buttonBase :: ExtendButtonBase ButtonBaseTypeMap 
buttonBase = _ButtonBase
foreign import _ButtonBase :: ExtendButtonBase ButtonBaseTypeMap 

type ButtonBaseProps = SimplifiedPropsOf ButtonBase

type ButtonBaseClassKey = Foreign

type ButtonBaseActions_required  optional =
  ( focusVisible :: Foreign
  | optional )

type ButtonBaseActions_optional =
  ( 
  )

foreign import data ButtonBaseActions :: Type 


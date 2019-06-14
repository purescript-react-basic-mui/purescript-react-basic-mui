module React.Basic.MUI.ButtonBase where 

import Prelude
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic.Events (EventHandler)
import React.Basic.MUI.ButtonBase.TouchRipple (TouchRippleProps)

type ButtonBaseTypeMap_required optional =
  ( props :: { action :: Foreign, buttonRef :: Foreign, centerRipple :: Boolean, disabled :: Boolean, disableRipple :: Boolean, disableTouchRipple :: Boolean, focusRipple :: Boolean, focusVisibleClassName :: String, onFocusVisible :: EventHandler, "TouchRippleProps" :: Foreign }
  , defaultComponent :: String
  , classKey :: Foreign
  | optional )

type ButtonBaseTypeMap_optional =
  ( 
  )

foreign import data ButtonBaseTypeMap :: Type 

buttonBaseTypeMap
  :: ∀ attrs attrs_
   . Union attrs attrs_ (ButtonBaseTypeMap_optional)
  => Record (ButtonBaseTypeMap_required attrs)
  -> ButtonBaseTypeMap
buttonBaseTypeMap = unsafeCoerce

type ExtendButtonBaseTypeMap_required optional m =
  ( props :: Foreign
  , defaultComponent :: Foreign
  , classKey :: Foreign
  | optional )

type ExtendButtonBaseTypeMap_optional m =
  ( 
  )

foreign import data ExtendButtonBaseTypeMap :: Type 

extendButtonBaseTypeMap
  :: ∀ attrs attrs_
   . Union attrs attrs_ (ExtendButtonBaseTypeMap_optional)
  => Record (ExtendButtonBaseTypeMap_required attrs)
  -> ExtendButtonBaseTypeMap
extendButtonBaseTypeMap = unsafeCoerce

type ExtendButtonBase m = Foreign

buttonBase :: ExtendButtonBase ButtonBaseTypeMap 
buttonBase = _ButtonBase
foreign import _ButtonBase :: ExtendButtonBase ButtonBaseTypeMap 

type ButtonBaseProps = SimplifiedPropsOf ButtonBase

type ButtonBaseClassKey = Foreign

type ButtonBaseActions_required optional =
  ( focusVisible :: Foreign
  | optional )

type ButtonBaseActions_optional =
  ( 
  )

foreign import data ButtonBaseActions :: Type 

buttonBaseActions
  :: ∀ attrs attrs_
   . Union attrs attrs_ (ButtonBaseActions_optional)
  => Record (ButtonBaseActions_required attrs)
  -> ButtonBaseActions
buttonBaseActions = unsafeCoerce
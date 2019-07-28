module MUI.Core.Tab where

import Foreign (Foreign)
import MUI.Core.ButtonBase (ButtonBaseActions, ButtonBaseTypeProp, TouchRippleProps)
import Prim.Row (class Union)
import React.Basic.DOM (Props_button)
import React.Basic.Events (EventHandler)
import React.Basic.Hooks (JSX, ReactComponent, Ref, element)
import Unsafe.Coerce (unsafeCoerce)

type TabProps value componentProps =
  ( action :: Ref ButtonBaseActions
  , buttonRef :: Ref Foreign
  , centerRipple :: Boolean
  , classes :: TabClassKey
  , component :: ReactComponent { | componentProps } 
  , disabled :: Boolean
  , disableRipple :: Boolean
  , disableTouchRipple :: Boolean
  , focusRipple :: Boolean
  , focusVisibleClassName :: String
  , icon :: JSX
  , label :: JSX
  , onFocusVisible :: EventHandler
  , "TouchRippleProps" :: TouchRippleProps
  , type :: ButtonBaseTypeProp
  , wrapped :: Boolean
  , value :: value
  | componentProps
  )

foreign import data TabPropsPartial :: Type

type TabClassKeyOptions =
  ( root :: String
  , labelIcon :: String
  , textColorInherit :: String
  , textColorPrimary :: String
  , textColorSecondary :: String
  , selected :: String
  , disabled :: String
  , fullWidth :: String
  , wrapped :: String
  , wrapper :: String
  )

foreign import data TabClassKey :: Type

tabClassKey :: ∀ options options_
  . Union options options_ TabClassKeyOptions
  => Record options
  -> TabClassKey
tabClassKey = unsafeCoerce

tabPartial_component :: ∀ value componentProps props props_
  . Union props props_ (TabProps value componentProps)
  => Record props 
  -> TabPropsPartial
tabPartial_component = unsafeCoerce

tabPartial :: ∀ value props props_
  . Union props props_ (TabProps value Props_button)
  => Record props 
  ->TabPropsPartial
tabPartial = unsafeCoerce


tab_component :: ∀ value componentProps props props_
  . Union props props_ (TabProps value componentProps)
  => Record props 
  -> JSX
tab_component = element _Tab

tab :: ∀ value props props_
  . Union props props_ (TabProps value Props_button)
  => Record props 
  -> JSX
tab = element _Tab


foreign import _Tab :: ∀ a. ReactComponent a
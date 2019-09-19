module MUI.Core.Tab where

import Foreign (Foreign)
import MUI.Core (JSS)
import MUI.Core.ButtonBase (ButtonBaseActions, ButtonBaseTypeProp)
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
  , "TouchRippleProps" :: Foreign
  , type :: ButtonBaseTypeProp
  , wrapped :: Boolean
  , value :: value
  | componentProps
  )

foreign import data TabPropsPartial :: Type

type TabClassKeyOptionsJSS = TabClassKeyOptionsR JSS
type TabClassKeyOptions = TabClassKeyOptionsR String
type TabClassKeyOptionsR a =
  ( root :: a
  , labelIcon :: a
  , textColorInherit :: a
  , textColorPrimary :: a
  , textColorSecondary :: a
  , selected :: a
  , disabled :: a
  , fullWidth :: a
  , wrapped :: a
  , wrapper :: a
  )

foreign import data TabClassKey :: Type
foreign import data TabClassKeyJSS :: Type

tabClassKey :: ∀ options options_
  . Union options options_ TabClassKeyOptions
  => Record options
  -> TabClassKey
tabClassKey = unsafeCoerce

tabClassKeyJSS :: ∀ options options_
  . Union options options_ TabClassKeyOptionsJSS
  => Record options
  -> TabClassKeyJSS
tabClassKeyJSS = unsafeCoerce

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

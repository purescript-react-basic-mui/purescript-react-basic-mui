module MUI.Core.IconButton where


import Foreign (Foreign)
import MUI.Core.ButtonBase (ButtonBaseActions, TouchRippleProps)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_button)
import React.Basic.Events (EventHandler)
import React.Basic.Hooks (Ref)
import Unsafe.Coerce (unsafeCoerce)

type IconButtonProps componentProps =
  ( children :: Array JSX
  , classes :: IconButtonClassKey
  , color :: String
  , edge :: String
  , size :: String
  , action :: Ref ButtonBaseActions
  , buttonRef :: Ref Foreign
  , centerRipple :: Boolean
  , component :: ReactComponent { | componentProps }
  , disabled :: Boolean
  , disableRipple :: Boolean
  , disableTouchRipple :: Boolean
  , focusRipple :: Boolean
  , focusVisibleClassName :: String
  , onFocusVisible :: EventHandler
  , "TouchRippleProps" :: TouchRippleProps
  , type :: String
  | componentProps
  )

foreign import data IconButtonClassKey :: Type
foreign import data IconButtonPropsPartial :: Type

type IconButtonClassKeyOptions =
  ( root :: String
  , edgeStart :: String
  , edgeEnd :: String
  , colorInherit :: String
  , colorPrimary :: String
  , colorSecondary :: String
  , disabled :: String
  , sizeSmall :: String
  , label :: String
  )

iconButtonClassKey :: ∀ options options_
  . Union options options_ IconButtonClassKeyOptions
  => Record options
  -> IconButtonClassKey
iconButtonClassKey = unsafeCoerce


iconButtonPropsPartial_component :: ∀ componentProps props props_
  . Union props props_ (IconButtonProps componentProps) 
  => Record props 
  -> IconButtonPropsPartial 
iconButtonPropsPartial_component = unsafeCoerce

iconButtonPropsPartial :: ∀ props props_
  . Union props props_ (IconButtonProps Props_button) 
  => Record props 
  -> IconButtonPropsPartial 
iconButtonPropsPartial = unsafeCoerce

iconButton_component :: ∀ componentProps props props_
  . Union props props_ (IconButtonProps componentProps)
  => Record props 
  -> JSX
iconButton_component = element _IconButton


iconButton :: ∀ props props_
  . Union props props_ (IconButtonProps Props_button)
  => Record props 
  -> JSX
iconButton = element _IconButton


foreign import _IconButton :: ∀ a. ReactComponent a
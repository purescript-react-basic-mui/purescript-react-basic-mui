module MUI.Core.CardActionArea where

import Foreign (Foreign)
import MUI.Core.ButtonBase (ButtonBaseActions, ButtonBaseTypeProp, TouchRippleProps)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_button)
import React.Basic.Events (EventHandler)
import React.Basic.Hooks (Ref)
import Unsafe.Coerce (unsafeCoerce)

type CardActionAreaProps componentProps =
  ( action :: Ref ButtonBaseActions
  , buttonRef :: Ref Foreign
  , centerRipple :: Boolean
  , classes :: CardActionAreaClassKey
  , component :: ReactComponent { | componentProps } 
  , disabled :: Boolean
  , disableRipple :: Boolean
  , disableTouchRipple :: Boolean
  , focusRipple :: Boolean
  , focusVisibleClassName :: String
  , onFocusVisible :: EventHandler
  , "TouchRippleProps" :: TouchRippleProps
  , type :: ButtonBaseTypeProp
  | componentProps
  )

foreign import data CardActionAreaClassKey :: Type
foreign import data CardActionAreaPropsPartial :: Type

type CardActionAreaClassKeyOptions =
  ( root :: String
  , focusVisible :: String
  , focusHighlight :: String
  )

cardActionAreaClassKey :: ∀ options options_
  . Union options options_ CardActionAreaClassKeyOptions
  => Record options
  -> CardActionAreaClassKey
cardActionAreaClassKey = unsafeCoerce

cardActionAreaPropsPartial_component :: ∀ componentProps props props_
  . Union props props_ (CardActionAreaProps componentProps)
  => Record props 
  -> CardActionAreaPropsPartial
cardActionAreaPropsPartial_component = unsafeCoerce

cardActionAreaPropsPartial :: ∀ props props_
  . Union props props_ (CardActionAreaProps Props_button)
  => Record props 
  -> CardActionAreaPropsPartial
cardActionAreaPropsPartial = unsafeCoerce

cardActionArea_component :: ∀ componentProps props props_
  . Union props props_ (CardActionAreaProps componentProps)
  => Record props 
  -> JSX
cardActionArea_component = element _CardActionArea

cardActionArea :: ∀ props props_
  . Union props props_ (CardActionAreaProps Props_button)
  => Record props 
  -> JSX
cardActionArea = element _CardActionArea


foreign import _CardActionArea :: ∀ a. ReactComponent a
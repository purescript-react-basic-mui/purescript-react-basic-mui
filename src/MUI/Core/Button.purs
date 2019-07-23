module MUI.Core.Button where

import Effect.Ref (Ref)
import Foreign (Foreign)
import MUI.Core.ButtonBase (ButtonBaseActions, ButtonBaseTypeProp, TouchRippleProps)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_button)
import React.Basic.Events (EventHandler)
import Unsafe.Coerce (unsafeCoerce)

type ButtonProps componentProps =
  ( action :: Ref ButtonBaseActions
  , buttonRef :: Ref Foreign
  , centerRipple :: Boolean
  , classes :: ButtonClassKey
  , color :: ColorProp
  , component :: ReactComponent { | componentProps } 
  , disabled :: Boolean
  , disableFocusRipple :: Boolean
  , disableRipple :: Boolean
  , disableTouchRipple :: Boolean
  , focusRipple :: Boolean
  , focusVisibleClassName :: String
  , fullWidth :: Boolean
  , href :: String
  , onFocusVisible :: EventHandler
  , size :: SizeProp
  , "TouchRippleProps" :: TouchRippleProps
  , type :: ButtonBaseTypeProp
  , variant :: VariantProp
  | componentProps
  )

foreign import data ColorProp :: Type
data Color =  Default | Inherit | Primary | Secondary
color :: Color -> ColorProp
color Default = unsafeCoerce "default"
color Inherit = unsafeCoerce "inherit"
color Primary = unsafeCoerce "primary"
color Secondary = unsafeCoerce "secondary"

foreign import data SizeProp :: Type
data Size = Small | Medium | Large
size :: Size -> SizeProp
size Small = unsafeCoerce "small"
size Medium = unsafeCoerce "medium"
size Large = unsafeCoerce "large"

foreign import data VariantProp :: Type
data Variant = Text | Outlined | Contained
variant :: Variant -> VariantProp
variant Text = unsafeCoerce "text"
variant Outlined = unsafeCoerce "outlined"
variant Contained = unsafeCoerce "contained"

type ButtonClassKeyOptions =
  ( root :: String
  , label :: String
  , text :: String
  , textPrimary :: String
  , textSecondary :: String
  , outlined :: String
  , outlinedPrimary :: String
  , outlinedSecondary :: String
  , contained :: String
  , containedPrimary :: String
  , containedSecondary :: String
  , focusVisible :: String
  , disabled :: String
  , colorInherit :: String
  , sizeSmall :: String
  , sizeLarge :: String
  , fullWidth :: String
  )

foreign import data ButtonClassKey :: Type
foreign import data ButtonPropsPartial :: Type

buttonClassKey :: ∀ options options_
  . Union options options_ ButtonClassKeyOptions
  => Record options
  -> ButtonClassKey
buttonClassKey = unsafeCoerce

buttonPropsPartial_component :: ∀ componentProps props props_
  . Union props props_ (ButtonProps componentProps)
  => Record props 
  -> ButtonPropsPartial
buttonPropsPartial_component = unsafeCoerce

buttonPropsPartial :: ∀ props props_
  . Union props props_ (ButtonProps Props_button)
  => Record props 
  -> ButtonPropsPartial
buttonPropsPartial = unsafeCoerce

button_component :: ∀ componentProps props props_
  . Union props props_ (ButtonProps componentProps)
  => Record props 
  -> JSX
button_component = element _Button

button :: ∀ props props_
  . Union props props_ (ButtonProps Props_button)
  => Record props 
  -> JSX
button = element _Button


foreign import _Button :: ∀ a. ReactComponent a
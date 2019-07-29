module MUI.Core.IconButton where


import Foreign (Foreign)
import MUI.Core (JSS)
import MUI.Core.ButtonBase (ButtonBaseActions, ButtonBaseTypeProp, TouchRippleProps)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_button)
import React.Basic.Events (EventHandler)
import React.Basic.Hooks (Ref)
import Unsafe.Coerce (unsafeCoerce)

type IconButtonProps componentProps =
  ( children :: Array JSX
  , classes :: IconButtonClassKey
  , color ::ColorProp
  , edge :: EdgeProp
  , size :: SizeProp
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
  , type :: ButtonBaseTypeProp 
  | componentProps
  )

foreign import data ColorProp :: Type
data Color = Primary | Secondary | Default | Inherit
color :: Color -> ColorProp
color Primary = unsafeCoerce "primary"
color Secondary = unsafeCoerce "secondary"
color Default = unsafeCoerce "default"
color Inherit = unsafeCoerce "inherit"

foreign import data EdgeProp :: Type
data Edge = Start | End | False
edge :: Edge -> EdgeProp
edge Start = unsafeCoerce "start"
edge End = unsafeCoerce "end"
edge False = unsafeCoerce false

foreign import data SizeProp :: Type
data Size = Small | Medium
size :: Size -> SizeProp
size Small = unsafeCoerce "small"
size Medium = unsafeCoerce "medium"

foreign import data IconButtonClassKey :: Type
foreign import data IconButtonClassKeyJSS :: Type
foreign import data IconButtonPropsPartial :: Type

type IconButtonClassKeyOptionsJSS = IconButtonClassKeyOptionsR JSS
type IconButtonClassKeyOptions = IconButtonClassKeyOptionsR String
type IconButtonClassKeyOptionsR a =
  ( root :: a
  , edgeStart :: a
  , edgeEnd :: a
  , colorInherit :: a
  , colorPrimary :: a
  , colorSecondary :: a
  , disabled :: a
  , sizeSmall :: a
  , label :: a
  ) 

iconButtonClassKey :: ∀ options options_
  . Union options options_ IconButtonClassKeyOptions
  => Record options
  -> IconButtonClassKey
iconButtonClassKey = unsafeCoerce

iconButtonClassKeyJSS :: ∀ options options_
  . Union options options_ IconButtonClassKeyOptionsJSS
  => Record options
  -> IconButtonClassKeyJSS
iconButtonClassKeyJSS = unsafeCoerce

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
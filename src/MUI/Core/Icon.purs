module MUI.Core.Icon where

import MUI.Core (JSS)
import MUI.Core.Icon.Color (ColorProp)
import MUI.Core.Icon.FontSize (FontSizeProp)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_span)
import Unsafe.Coerce (unsafeCoerce)

type IconProps componentProps =
  ( children :: (Array JSX)
  , classes :: IconClassKey
  , color :: ColorProp
  , component :: ReactComponent  { | componentProps }
  , fontSize :: FontSizeProp
  | componentProps
  )

type IconClassKeyOptionsJSS = IconClassKeyOptionsR JSS 
type IconClassKeyOptions = IconClassKeyOptionsR String
type IconClassKeyOptionsR a =
  ( root :: a
  , colorPrimary :: a
  , colorSecondary :: a
  , colorAction :: a
  , colorError :: a
  , colorDisabled :: a
  , fontSizeInherit :: a
  , fontSizeSmall :: a
  , fontSizeLarge :: a
  )

foreign import data IconClassKey :: Type
foreign import data IconClassKeyJSS :: Type
foreign import data IconPropsPartial :: Type

iconClassKey :: ∀ options options_
  .  Union options options_ IconClassKeyOptions
  => Record options
  -> IconClassKey 
iconClassKey = unsafeCoerce

iconClassKeyJSS :: ∀ options options_
  .  Union options options_ IconClassKeyOptionsJSS
  => Record options
  -> IconClassKeyJSS 
iconClassKeyJSS = unsafeCoerce

iconPropsPartial :: ∀ props props_
  .  Union props props_ (IconProps Props_span)
  => Record props 
  -> IconPropsPartial
iconPropsPartial = unsafeCoerce

iconPropsPartial_component :: ∀ componentProps props props_
  .  Union props props_ (IconProps componentProps)
  => Record props 
  -> IconPropsPartial
iconPropsPartial_component = unsafeCoerce

icon_component :: ∀ componentProps props props_
  .  Union props props_ (IconProps componentProps)
  => Record props 
  -> JSX
icon_component = element _Icon

icon :: ∀ props props_
  .  Union props props_ (IconProps Props_span)
  => Record props 
  -> JSX
icon = element _Icon


foreign import _Icon :: ∀ a. ReactComponent a
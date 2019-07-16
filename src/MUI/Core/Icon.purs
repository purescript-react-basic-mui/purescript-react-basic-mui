module MUI.Core.Icon where

import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_span)
import Unsafe.Coerce (unsafeCoerce)

type IconProps componentProps =
  ( children :: (Array JSX)
  , classes :: IconClassKey
  , color :: String
  , component :: ReactComponent  { | componentProps }
  , fontSize :: String
  | componentProps
  )

type IconClassKeyOptions =
  ( root :: String
  , colorPrimary :: String
  , colorSecondary :: String
  , colorAction :: String
  , colorError :: String
  , colorDisabled :: String
  , fontSizeInherit :: String
  , fontSizeSmall :: String
  , fontSizeLarge :: String
  )

foreign import data IconClassKey :: Type
foreign import data IconPropsPartial :: Type

iconClassKey :: ∀ options options_
  .  Union options options_ IconClassKeyOptions
  => Record options
  -> IconClassKey 
iconClassKey = unsafeCoerce

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
module MUI.Core.Icon where

import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import Unsafe.Coerce (unsafeCoerce)

type IconProps =
  ( children :: (Array JSX)
  , classes :: IconClassKey
  , color :: String
  , component :: String
  , fontSize :: String
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
  .  Union props props_ IconProps
  => Record props 
  -> IconPropsPartial
iconPropsPartial = unsafeCoerce

icon :: ∀ props props_
  .  Union props props_ IconProps
  => Record props 
  -> JSX
icon = element _Icon

foreign import _Icon :: ∀ a. ReactComponent a
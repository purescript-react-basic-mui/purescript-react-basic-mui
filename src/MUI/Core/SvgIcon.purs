module MUI.Core.SvgIcon where

import React.Basic (JSX, ReactComponent, element)
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)

type SvgIconProps =
  ( children :: Array JSX
  , classes :: SvgIconClassKey
  , color :: String
  , component :: String
  , fontSize :: String
  , htmlColor :: String
  , shapeRendering :: String
  , titleAccess :: String
  , viewBox :: String
  )

foreign import data SvgIconClassKey :: Type
foreign import data SvgIconPropsPartial :: Type

type SvgIconClassKeyOptions =
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

svgIconClassKey :: ∀ options options_
  . Union options options_ SvgIconClassKeyOptions
  => Record options
  -> SvgIconClassKey
svgIconClassKey = unsafeCoerce

svgIconPropsPartial :: ∀ props props_
  . Union props props_ SvgIconProps
  => Record props 
  -> SvgIconPropsPartial 
svgIconPropsPartial = unsafeCoerce

svgIcon :: ∀ props props_
  . Union props props_ SvgIconProps
  => Record props 
  -> JSX
svgIcon = element _SvgIcon


foreign import _SvgIcon :: ∀ a. ReactComponent a
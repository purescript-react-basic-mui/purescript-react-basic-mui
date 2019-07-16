module MUI.Core.SvgIcon where

import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_svg)
import Unsafe.Coerce (unsafeCoerce)

type SvgIconProps componentProps =
  ( children :: Array JSX
  , classes :: SvgIconClassKey
  , color :: String
  , component :: ReactComponent { | componentProps }
  , fontSize :: String
  , htmlColor :: String
  , shapeRendering :: String
  , titleAccess :: String
  , viewBox :: String
  | componentProps
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

svgIconPropsPartial_component :: ∀ componentProps props props_
  . Union props props_ (SvgIconProps componentProps)
  => Record props 
  -> SvgIconPropsPartial 
svgIconPropsPartial_component = unsafeCoerce

svgIconPropsPartial :: ∀ props props_
  . Union props props_ (SvgIconProps Props_svg)
  => Record props 
  -> SvgIconPropsPartial 
svgIconPropsPartial = unsafeCoerce


svgIcon_component :: ∀ componentProps props props_
  . Union props props_ (SvgIconProps componentProps)
  => Record props 
  -> JSX
svgIcon_component = element _SvgIcon

svgIcon :: ∀ props props_
  . Union props props_ (SvgIconProps Props_svg)
  => Record props 
  -> JSX
svgIcon = element _SvgIcon



foreign import _SvgIcon :: ∀ a. ReactComponent a
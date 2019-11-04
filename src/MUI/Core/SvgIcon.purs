module MUI.Core.SvgIcon where

import MUI.Core (JSS)
import MUI.Core.Icon.Color as Icon
import MUI.Core.Icon.FontSize (FontSize)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM.Internal (SharedSVGProps)
import React.Basic.DOM.SVG (Props_svg)
import Unsafe.Coerce (unsafeCoerce)

type SvgIconProps componentProps =
  ( children :: Array JSX
  , classes :: SvgIconClassKey
  , color :: Icon.ColorProp
  , component :: ReactComponent { | componentProps }
  , fontSize :: FontSize
  , htmlColor :: String
  , shapeRendering :: String
  , titleAccess :: String
  , viewBox :: String
  | componentProps
  )

foreign import data SvgIconClassKey :: Type
foreign import data SvgIconClassKeyJSS :: Type
foreign import data SvgIconPropsPartial :: Type

type SvgIconClassKeyOptionsJSS = SvgIconClassKeyOptionsR JSS
type SvgIconClassKeyOptions = SvgIconClassKeyOptionsR String
type SvgIconClassKeyOptionsR a =
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

svgIconClassKey :: ∀ options options_
  . Union options options_ SvgIconClassKeyOptions
  => Record options
  -> SvgIconClassKey
svgIconClassKey = unsafeCoerce

svgIconClassKeyJSS :: ∀ options options_
  . Union options options_ SvgIconClassKeyOptionsJSS
  => Record options
  -> SvgIconClassKeyJSS
svgIconClassKeyJSS = unsafeCoerce

svgIconPropsPartial_component :: ∀ componentProps props props_
  . Union props props_ (SvgIconProps componentProps)
  => Record props 
  -> SvgIconPropsPartial 
svgIconPropsPartial_component = unsafeCoerce

svgIconPropsPartial :: ∀ props props_
  . Union props props_ (SvgIconProps (SharedSVGProps Props_svg))
  => Record props 
  -> SvgIconPropsPartial 
svgIconPropsPartial = unsafeCoerce


svgIcon_component :: ∀ componentProps props props_
  . Union props props_ (SvgIconProps componentProps)
  => Record props 
  -> JSX
svgIcon_component = element _SvgIcon

svgIcon :: ∀ props props_
  . Union props props_ (SvgIconProps (SharedSVGProps Props_svg))
  => Record props 
  -> JSX
svgIcon = element _SvgIcon



foreign import _SvgIcon :: ∀ a. ReactComponent a

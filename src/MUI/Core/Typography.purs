module MUI.Core.Typography where

import MUI.Core (JSS)
import MUI.Core.Typography.Align (AlignProp)
import MUI.Core.Typography.Color (ColorProp)
import MUI.Core.Typography.Display (DisplayProp)
import MUI.Core.Typography.Variant (VariantProp)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_h1)
import Unsafe.Coerce (unsafeCoerce)

type TypographyPropsOptions componentProps = 
  ( align :: AlignProp
  , children :: (Array JSX)
  , classes :: TypographyClassKey
  , color :: ColorProp
  , component :: ReactComponent { | componentProps }
  , display :: DisplayProp
  , gutterBottom :: Boolean
  , noWrap :: Boolean
  , paragraph :: Boolean
  , variant :: VariantProp
  , variantMapping :: VariantMapping
  | componentProps
  )

foreign import data TypographyProps :: Type

type VariantMapping =
  { h1 :: String
  , h2 :: String
  , h3 :: String
  , h4 :: String
  , h5 :: String
  , h6 :: String
  , subtitle1 :: String
  , subtitle2 :: String
  , body1 :: String
  , body2 :: String
  }


type TypographyClassKeyGenericOptions a =
  ( root :: a 
  , body2 :: a 
  , body1 :: a 
  , caption :: a 
  , button :: a 
  , h1 :: a 
  , h2 :: a 
  , h3 :: a 
  , h4 :: a 
  , h5 :: a 
  , h6 :: a 
  , subtitle1 :: a 
  , subtitle2 :: a 
  , overline :: a 
  , srOnly :: a 
  , alignLeft :: a 
  , alignCenter :: a 
  , alignRight :: a 
  , alignJustify :: a 
  , noWrap :: a 
  , gutterBottom :: a 
  , paragraph :: a 
  , colorInherit :: a 
  , colorPrimary :: a 
  , colorSecondary :: a 
  , colorTextPrimary :: a 
  , colorTextSecondary :: a 
  , colorError :: a 
  , displayInline :: a 
  , displayBlock :: a 
  )
type TypographyClassKeyOptions = TypographyClassKeyGenericOptions String
type TypographyClassKeyJSSOptions = TypographyClassKeyGenericOptions JSS
foreign import data TypographyClassKey :: Type
foreign import data TypographyClassKeyJSS :: Type

typographyClassKey :: ∀  given required
  .  Union given required (TypographyClassKeyOptions )
  => Record given
  -> TypographyClassKey
typographyClassKey = unsafeCoerce

typographyClassKeyJSS :: ∀  given required
  .  Union given required (TypographyClassKeyJSSOptions )
  => Record given
  -> TypographyClassKeyJSS
typographyClassKeyJSS = unsafeCoerce

typography :: ∀  given required
  .  Union given required (TypographyPropsOptions Props_h1 )
  => Record given
  -> JSX
typography = element _Typography

typography_component :: ∀ componentProps given required
  .  Union given required (TypographyPropsOptions componentProps)
  => Record given
  -> JSX
typography_component = element _Typography

foreign import _Typography :: ∀ a. ReactComponent a
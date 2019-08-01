module MUI.Core.Badge where

import Prelude

import MUI.Core (JSS)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_div)
import Unsafe.Coerce (unsafeCoerce)

type BadgePropsOptions componentProps = 
  ( badgeContent :: JSX
  , children :: (Array JSX)
  , classes :: BadgeClassKey
  , color :: ColorProp
  , component :: ReactComponent { | componentProps }
  , invisible :: Boolean
  , max :: Number
  , showZero :: Boolean
  , variant :: VariantProp
  | componentProps
  )

foreign import data BadgeProps :: Type

foreign import data ColorProp :: Type
foreign import _eqColorProp :: ColorProp -> ColorProp -> Boolean
foreign import _ordColorProp :: ColorProp -> ColorProp -> Int
instance eqColorProp :: Eq ColorProp where eq left right = _eqColorProp left right
instance ordColorProp :: Ord ColorProp where compare left right = compare (_ordColorProp left right) (_ordColorProp right left)

default :: ColorProp
default = unsafeCoerce "default"

primary :: ColorProp
primary = unsafeCoerce "primary"

secondary :: ColorProp
secondary = unsafeCoerce "secondary"

error :: ColorProp
error = unsafeCoerce "error"
foreign import data VariantProp :: Type
foreign import _eqVariantProp :: VariantProp -> VariantProp -> Boolean
foreign import _ordVariantProp :: VariantProp -> VariantProp -> Int
instance eqVariantProp :: Eq VariantProp where eq left right = _eqVariantProp left right
instance ordVariantProp :: Ord VariantProp where compare left right = compare (_ordVariantProp left right) (_ordVariantProp right left)

standard :: VariantProp
standard = unsafeCoerce "standard"

dot :: VariantProp
dot = unsafeCoerce "dot"

type BadgeClassKeyGenericOptions a =
  ( root :: a 
  , badge :: a 
  , colorPrimary :: a 
  , colorSecondary :: a 
  , colorError :: a 
  , invisible :: a 
  , dot :: a 
  )
type BadgeClassKeyOptions = BadgeClassKeyGenericOptions String
type BadgeClassKeyJSSOptions = BadgeClassKeyGenericOptions JSS
foreign import data BadgeClassKey :: Type
foreign import data BadgeClassKeyJSS :: Type

badgeClassKey :: ∀  given required
  .  Union given required (BadgeClassKeyOptions )
  => Record given
  -> BadgeClassKey
badgeClassKey = unsafeCoerce

badgeClassKeyJSS :: ∀  given required
  .  Union given required (BadgeClassKeyJSSOptions )
  => Record given
  -> BadgeClassKeyJSS
badgeClassKeyJSS = unsafeCoerce

badge :: ∀  given required
  .  Union given required (BadgePropsOptions Props_div )
  => Record given
  -> JSX
badge = element _Badge

badge_component :: ∀ componentProps given required
  .  Union given required (BadgePropsOptions componentProps)
  => Record given
  -> JSX
badge_component = element _Badge

foreign import _Badge :: ∀ a. ReactComponent a
module MUI.Core.Badge where

import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_span)
import Unsafe.Coerce (unsafeCoerce)

type BadgeProps componentProps =
  ( badgeContent :: JSX
  , children :: Array JSX
  , classes :: BadgeClassKey
  , color :: ColorProp
  , component :: ReactComponent { | componentProps }
  , invisible :: Boolean
  , max :: Number
  , showZero :: Boolean
  , variant :: VariantProp
  | componentProps 
  )

foreign import data BadgePropsPartial :: Type

foreign import data ColorProp :: Type
data Color = Default | Primary | Secondary | Error
color :: Color -> ColorProp
color Default = unsafeCoerce "default"
color Primary = unsafeCoerce "primary"
color Secondary = unsafeCoerce "secondary"
color Error = unsafeCoerce "error"

foreign import data VariantProp :: Type
data Variant = Standard | Dot
variant :: Variant -> VariantProp
variant Standard = unsafeCoerce "standard"
variant Dot = unsafeCoerce "dot"


type BadgeClassKeyOptions =
  ( root :: String
  , badge :: String
  , colorPrimary :: String
  , colorSecondary :: String
  , colorError :: String
  , invisible :: String
  , dot :: String
  )

foreign import data BadgeClassKey :: Type

badgeClassKey :: ∀ options options_
  . Union options options_ BadgeClassKeyOptions
  => Record options
  -> BadgeClassKey
badgeClassKey = unsafeCoerce

badgePropsPartial_component :: ∀ componentProps props props_
  . Union props props_ (BadgeProps componentProps)
  => Record props 
  -> BadgePropsPartial
badgePropsPartial_component = unsafeCoerce

badgePropsPartial :: ∀ props props_
  . Union props props_ (BadgeProps Props_span)
  => Record props 
  -> BadgePropsPartial
badgePropsPartial = unsafeCoerce

badge_component :: ∀ componentProps props props_
  . Union props props_ (BadgeProps componentProps)
  => Record props 
  -> JSX
badge_component = element _Badge

badge :: ∀ props props_
  . Union props props_ (BadgeProps Props_span)
  => Record props 
  -> JSX
badge = element _Badge


foreign import _Badge :: ∀ a. ReactComponent a
module MUI.Core.Link where


import MUI.Core.Typography (TypographyClassKey, VariantMapping)
import MUI.Core.Typography.Align (AlignProp)
import MUI.Core.Typography.Display (DisplayProp)
import MUI.Core.Typography.Variant (VariantProp)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_a)
import Unsafe.Coerce (unsafeCoerce)

type LinkProps componentProps =
  ( align :: AlignProp
  , children :: Array JSX
  , classes :: LinkClassKey
  , color :: ColorProp
  , component :: ReactComponent { | componentProps }
  , display :: DisplayProp
  , gutterBottom :: Boolean
  , noWrap :: Boolean
  , paragraph :: Boolean
  , "TypographyClasses" :: TypographyClassKey
  , underline :: UnderlineProp
  , variant :: VariantProp
  , variantMapping :: VariantMapping
  | componentProps
  )

foreign import data ColorProp :: Type
data Color = Default | Error | Inherit | Primary | Secondary | TextPrimary | TextSecondary
color :: Color -> ColorProp
color Default = unsafeCoerce "default"
color Error = unsafeCoerce "error"
color Inherit = unsafeCoerce "inherit"
color Primary = unsafeCoerce "primary"
color Secondary = unsafeCoerce "secondary"
color TextPrimary = unsafeCoerce "textPrimary"
color TextSecondary = unsafeCoerce "textSecondary"

foreign import data UnderlineProp :: Type
data Underline = None | Hover | Always
underline :: Underline -> UnderlineProp
underline None = unsafeCoerce "none"
underline Hover = unsafeCoerce "hover"
underline Always = unsafeCoerce "Always"

foreign import data LinkClassKey :: Type
foreign import data LinkPropsPartial :: Type

type LinkClassKeyOptions =
  ( root :: String
  , underlineNone :: String
  , underlineHover :: String
  , underlineAlways :: String
  , button :: String
  , focusVisible :: String
  )

linkClassKey  :: ∀ options options_
  . Union options options_ LinkClassKeyOptions
  => Record options
  -> LinkClassKey
linkClassKey = unsafeCoerce

linkPropsPartial_component :: ∀ componentProps props props_
  . Union props props_ (LinkProps componentProps)
  => Record props 
  -> LinkPropsPartial
linkPropsPartial_component = unsafeCoerce

linkPropsPartial :: ∀ props props_
  . Union props props_ (LinkProps Props_a)
  => Record props 
  -> LinkPropsPartial
linkPropsPartial = unsafeCoerce


link_component :: ∀ componentProps props props_
  . Union props props_ (LinkProps componentProps)
  => { | props }
  -> JSX
link_component = element _Link

link :: ∀ props props_
  . Union props props_ (LinkProps Props_a)
  => { | props }
  -> JSX
link = element _Link

foreign import _Link :: ∀ a. ReactComponent a

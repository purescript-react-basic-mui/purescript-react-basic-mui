module MUI.Core.ButtonGroup where

import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_div)
import Unsafe.Coerce (unsafeCoerce)

type ButtonGroupProps componentProps =
  ( children :: Array JSX
  , classes :: ButtonGroupClassKey
  , color :: ColorProp
  , component :: ReactComponent { | componentProps }
  , disabled :: Boolean
  , disableFocusRipple :: Boolean
  , disableRipple :: Boolean
  , fullWidth :: Boolean
  , size :: SizeProp
  , variant :: VariantProp
  | componentProps
  )

foreign import data ColorProp :: Type
data Color =  Default | Inherit | Primary | Secondary
color :: Color -> ColorProp
color Default = unsafeCoerce "default"
color Inherit = unsafeCoerce "inherit"
color Primary = unsafeCoerce "primary"
color Secondary = unsafeCoerce "secondary"

foreign import data SizeProp :: Type
data Size = Small | Medium | Large
size :: Size -> SizeProp
size Small = unsafeCoerce "small"
size Medium = unsafeCoerce "medium"
size Large = unsafeCoerce "large"

foreign import data VariantProp :: Type
data Variant = Outlined | Contained
variant :: Variant -> VariantProp
variant Outlined = unsafeCoerce "outlined"
variant Contained = unsafeCoerce "contained"


foreign import data ButtonGroupClassKey :: Type
foreign import data ButtonGroupPropsPartial :: Type

type ButtonGroupClassKeyOptions =
  ( root :: String
  , contained :: String
  , fullWidth :: String
  , grouped :: String
  , groupedOutlined :: String
  , groupedOutlinedPrimary :: String
  , groupedOutlinedSecondary :: String
  , groupedContained :: String
  , groupedContainedPrimary :: String
  , groupedContainedSecondary :: String
  )

buttonGroupClassKey :: ∀ options options_
  . Union options options_ ButtonGroupClassKeyOptions
  => Record options
  -> ButtonGroupClassKey
buttonGroupClassKey = unsafeCoerce

buttonGroupPropsPartial_component :: ∀ componentProps props props_
  . Union props props_ (ButtonGroupProps componentProps)
  => Record props 
  -> ButtonGroupPropsPartial
buttonGroupPropsPartial_component = unsafeCoerce

buttonGroupPropsPartial :: ∀ props props_
  . Union props props_ (ButtonGroupProps Props_div)
  => Record props 
  -> ButtonGroupPropsPartial
buttonGroupPropsPartial = unsafeCoerce

buttonGroup_component :: ∀ componentProps props props_
  . Union props props_ (ButtonGroupProps componentProps)
  => Record props 
  -> JSX
buttonGroup_component = element _ButtonGroup

buttonGroup :: ∀ props props_
  . Union props props_ (ButtonGroupProps Props_div)
  => Record props 
  -> JSX
buttonGroup = element _ButtonGroup


foreign import _ButtonGroup :: ∀ a. ReactComponent a
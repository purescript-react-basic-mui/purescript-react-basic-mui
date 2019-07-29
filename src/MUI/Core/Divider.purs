module MUI.Core.Divider where

import MUI.Core (JSS)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_hr)
import Unsafe.Coerce (unsafeCoerce)

type DividerProps componentProps =
  ( absolute :: Boolean
  , classes :: DividerClassKey 
  , component :: ReactComponent { | componentProps }
  , light :: Boolean
  , variant :: VariantProp
  | componentProps
  )

foreign import data VariantProp :: Type

data Variant = FullWidth | Inset | Middle 

variant :: Variant -> VariantProp
variant FullWidth = unsafeCoerce "fullWidth"
variant Inset = unsafeCoerce "inset"
variant Middle = unsafeCoerce "middle"


foreign import data DividerClassKey :: Type
foreign import data DividerClassKeyJSS :: Type
foreign import data DividerPropsPartial :: Type

type DividerClassKeyOptionsJSS = DividerClassKeyOptionsR JSS
type DividerClassKeyOptions = DividerClassKeyOptionsR String
type DividerClassKeyOptionsR a =
  ( root :: a
  , absolute :: a
  , inset :: a
  , light :: a
  , middle :: a
  )

dividerClassKey :: ∀ options options_
  . Union options options_ DividerClassKeyOptions
  => Record options
  -> DividerClassKey
dividerClassKey = unsafeCoerce

dividerClassKeyJSS :: ∀ options options_
  . Union options options_ DividerClassKeyOptionsJSS
  => Record options
  -> DividerClassKeyJSS
dividerClassKeyJSS = unsafeCoerce

dividerPropsPartial_component :: ∀ componentProps props props_
  . Union props props_ (DividerProps componentProps)
  => Record props 
  -> DividerPropsPartial 
dividerPropsPartial_component = unsafeCoerce

dividerPropsPartial :: ∀ props props_
  . Union props props_ (DividerProps Props_hr)
  => Record props 
  -> DividerPropsPartial 
dividerPropsPartial = unsafeCoerce

divider_component :: ∀ componentProps props props_
  . Union props props_ (DividerProps componentProps)
  => Record props 
  -> JSX
divider_component = element _Divider

divider :: ∀ props props_
  . Union props props_ (DividerProps Props_hr)
  => Record props 
  -> JSX
divider = element _Divider


foreign import _Divider :: ∀ a. ReactComponent a
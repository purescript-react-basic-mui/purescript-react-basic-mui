module MUI.Core.Divider where

import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_hr)
import Unsafe.Coerce (unsafeCoerce)

type DividerProps componentProps =
  ( absolute :: Boolean
  , classes :: DividerClassKey 
  , component :: ReactComponent { | componentProps }
  , light :: Boolean
  , variant :: String
  | componentProps
  )

foreign import data DividerClassKey :: Type
foreign import data DividerPropsPartial :: Type

type DividerClassKeyOptions =
  ( root :: String
  , absolute :: String
  , inset :: String
  , light :: String
  , middle :: String
  )

dividerClassKey :: ∀ options options_
  . Union options options_ DividerClassKeyOptions
  => Record options
  -> DividerClassKey
dividerClassKey = unsafeCoerce

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
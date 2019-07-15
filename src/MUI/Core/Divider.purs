module MUI.Core.Divider where

import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_hr)
import Unsafe.Coerce (unsafeCoerce)

type DividerProps a =
  ( absolute :: Boolean
  , classes :: DividerClassKey 
  , component :: ReactComponent { | a }
  , light :: Boolean
  , variant :: String
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

dividerPropsPartial :: ∀ props props_
  . Union props props_ (DividerProps Props_hr)
  => Record props 
  -> DividerPropsPartial 
dividerPropsPartial = unsafeCoerce

divider :: ∀ props props_
  . Union props props_ (DividerProps Props_hr)
  => Record props 
  -> JSX
divider = element _Divider

foreign import _Divider :: ∀ a. ReactComponent a
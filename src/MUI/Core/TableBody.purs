module MUI.Core.TableBody where

import MUI.Core (JSS)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_tbody)
import Unsafe.Coerce (unsafeCoerce)

type TableBodyProps componentProps =
  ( children :: Array JSX
  , classes :: TableBodyClassKey
  , component :: ReactComponent { | componentProps }
  | componentProps
  )

foreign import data TableBodyClassKey :: Type
foreign import data TableBodyClassKeyJSS :: Type
foreign import data TableBodyPropsPartial :: Type

type TableBodyClassKeyOptionsJSS = TableBodyClassKeyOptionsR JSS
type TableBodyClassKeyOptions = TableBodyClassKeyOptionsR String
type TableBodyClassKeyOptionsR a = ( root :: a )

tableBodyClassKey :: ∀ options options_
  . Union options options_ TableBodyClassKeyOptions
  => Record options
  -> TableBodyClassKey
tableBodyClassKey = unsafeCoerce

tableBodyClassKeyJSS :: ∀ options options_
  . Union options options_ TableBodyClassKeyOptionsJSS
  => Record options
  -> TableBodyClassKeyJSS
tableBodyClassKeyJSS = unsafeCoerce

tableBodyPropsPartial_component :: ∀ componentProps props props_
  . Union props props_ (TableBodyProps componentProps)
  => Record props 
  -> TableBodyPropsPartial
tableBodyPropsPartial_component = unsafeCoerce

tableBodyPropsPartial :: ∀ props props_
  . Union props props_ (TableBodyProps Props_tbody)
  => Record props 
  -> TableBodyPropsPartial
tableBodyPropsPartial = unsafeCoerce

tableBody_component :: ∀ componentProps props props_
  . Union props props_ (TableBodyProps componentProps)
  => Record props 
  -> JSX
tableBody_component = element _TableBody

tableBody :: ∀ props props_
  . Union props props_ (TableBodyProps Props_tbody)
  => Record props 
  -> JSX
tableBody = element _TableBody


foreign import  _TableBody :: ∀ a. ReactComponent a

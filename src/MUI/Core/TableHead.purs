module MUI.Core.TableHead where

import MUI.Core (JSS)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_thead)
import Unsafe.Coerce (unsafeCoerce)

type TableHeadProps componentProps =
  ( children :: Array JSX
  , classes :: TableHeadClassKey
  , component :: ReactComponent { | componentProps } 
  | componentProps
  )

foreign import data TableHeadClassKey :: Type
foreign import data TableHeadClassKeyJSS :: Type
foreign import data TableHeadPropsPartial :: Type

type TableHeadClassKeyOptionsJSS = TableHeadClassKeyOptionsR JSS
type TableHeadClassKeyOptions = TableHeadClassKeyOptionsR String
type TableHeadClassKeyOptionsR a = ( root :: a )

tableHeadClassKey :: ∀ options options_
  . Union options options_ TableHeadClassKeyOptions
  => Record options
  -> TableHeadClassKey
tableHeadClassKey = unsafeCoerce

tableHeadClassKeyJSS :: ∀ options options_
  . Union options options_ TableHeadClassKeyOptionsJSS
  => Record options
  -> TableHeadClassKeyJSS
tableHeadClassKeyJSS = unsafeCoerce

tableHeadPropsPartial_component :: ∀ componentProps props props_
  . Union props props_ (TableHeadProps componentProps)
  => Record props 
  -> TableHeadPropsPartial 
tableHeadPropsPartial_component = unsafeCoerce

tableHeadPropsPartial :: ∀ props props_
  . Union props props_ (TableHeadProps Props_thead)
  => Record props 
  -> TableHeadPropsPartial 
tableHeadPropsPartial = unsafeCoerce

tableHead_component :: ∀ componentProps props props_
  . Union props props_ (TableHeadProps componentProps)
  => Record props 
  -> JSX
tableHead_component = element _TableHead

tableHead :: ∀ props props_
  . Union props props_ (TableHeadProps Props_thead)
  => Record props 
  -> JSX
tableHead = element _TableHead

foreign import  _TableHead :: ∀ a. ReactComponent a

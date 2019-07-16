module MUI.Core.TableHead where

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
foreign import data TableHeadPropsPartial :: Type

type TableHeadClassKeyOptions = ( root :: String )

tableHeadClassKey :: ∀ options options_
  . Union options options_ TableHeadClassKeyOptions
  => Record options
  -> TableHeadClassKey
tableHeadClassKey = unsafeCoerce

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

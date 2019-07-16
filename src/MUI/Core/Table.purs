module MUI.Core.Table where

import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_table)
import Unsafe.Coerce (unsafeCoerce)

type TableProps componentProps =
  ( children :: Array JSX
  , classes :: TableClassKey
  , component :: ReactComponent { | componentProps }
  , padding :: String
  , size :: String
  | componentProps
  )

foreign import data TableClassKey :: Type
foreign import data TablePropsPartial :: Type

type TableClassKeyOptions = ( root :: String )

tableClassKey :: ∀ options options_
  .  Union options options_ TableClassKeyOptions
  => Record options
  -> TableClassKey
tableClassKey = unsafeCoerce

tablePropsPartial_component :: ∀ componentProps props props_
  .  Union props props_ (TableProps componentProps)
  => Record props 
  -> TablePropsPartial 
tablePropsPartial_component = unsafeCoerce

tablePropsPartial :: ∀ props props_
  .  Union props props_ (TableProps Props_table)
  => Record props 
  -> TablePropsPartial 
tablePropsPartial = unsafeCoerce

table_component :: ∀ componentProps props props_
  .  Union props props_ (TableProps componentProps)
  => Record props 
  -> JSX
table_component = element _Table

table :: ∀ props props_
  .  Union props props_ (TableProps Props_table)
  => Record props 
  -> JSX
table = element _Table

foreign import  _Table :: ∀ a. ReactComponent a
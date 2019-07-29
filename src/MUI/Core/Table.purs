module MUI.Core.Table where

import MUI.Core (JSS)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_table)
import Unsafe.Coerce (unsafeCoerce)

type TableProps componentProps =
  ( children :: Array JSX
  , classes :: TableClassKey
  , component :: ReactComponent { | componentProps }
  , padding :: PaddingProp
  , size :: SizeProp
  | componentProps
  )

foreign import data PaddingProp :: Type
data Padding = Default | Checkbox | None
padding :: Padding -> PaddingProp
padding Default = unsafeCoerce "default"
padding Checkbox = unsafeCoerce "checkbox"
padding None = unsafeCoerce "none"

foreign import data SizeProp :: Type
data Size = Small | Medium
size :: Size -> SizeProp
size Small = unsafeCoerce "small"
size Medium = unsafeCoerce "medium"

foreign import data TableClassKey :: Type
foreign import data TableClassKeyJSS :: Type
foreign import data TablePropsPartial :: Type

type TableClassKeyOptionsJSS = TableClassKeyOptionsR JSS 
type TableClassKeyOptions = TableClassKeyOptionsR String
type TableClassKeyOptionsR a = ( root :: a )

tableClassKey :: ∀ options options_
  .  Union options options_ TableClassKeyOptions
  => Record options
  -> TableClassKey
tableClassKey = unsafeCoerce

tableClassKeyJSS :: ∀ options options_
  .  Union options options_ TableClassKeyOptionsJSS
  => Record options
  -> TableClassKeyJSS
tableClassKeyJSS = unsafeCoerce

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
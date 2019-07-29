module MUI.Core.TableRow where

import MUI.Core (JSS)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_tr)
import Unsafe.Coerce (unsafeCoerce)

type TableRowProps componentProps =
  ( children :: Array JSX
  , classes :: TableRowClassKey
  , component :: ReactComponent { | componentProps }
  , hover :: Boolean
  , selected :: Boolean
  | componentProps
  )

foreign import data TableRowClassKey :: Type
foreign import data TableRowClassKeyJSS :: Type
foreign import data TableRowPropsPartial :: Type

type TableRowClassKeyOptionsJSS = TableRowClassKeyOptionsR JSS
type TableRowClassKeyOptions = TableRowClassKeyOptionsR String
type TableRowClassKeyOptionsR a = 
  ( root :: a
  , selected :: a
  , hover :: a
  , head :: a
  , footer :: a
  )

tableRowClassKey :: ∀ options options_
  .  Union options options_ TableRowClassKeyOptions
  => Record options
  -> TableRowClassKey
tableRowClassKey = unsafeCoerce

tableRowClassKeyJSS :: ∀ options options_
  .  Union options options_ TableRowClassKeyOptionsJSS
  => Record options
  -> TableRowClassKeyJSS
tableRowClassKeyJSS = unsafeCoerce

tableRowPropsPartial_component :: ∀ componentProps props props_
  .  Union props props_ (TableRowProps componentProps)
  => Record props 
  -> TableRowPropsPartial
tableRowPropsPartial_component = unsafeCoerce

tableRowPropsPartial :: ∀ props props_
  .  Union props props_ (TableRowProps Props_tr)
  => Record props 
  -> TableRowPropsPartial
tableRowPropsPartial = unsafeCoerce

tableRow_component :: ∀ componentProps props props_
  .  Union props props_ (TableRowProps componentProps)
  => Record props 
  -> JSX
tableRow_component = element _TableRow

tableRow :: ∀ props props_
  .  Union props props_ (TableRowProps Props_tr)
  => Record props 
  -> JSX
tableRow = element _TableRow

foreign import  _TableRow :: ∀ a. ReactComponent a

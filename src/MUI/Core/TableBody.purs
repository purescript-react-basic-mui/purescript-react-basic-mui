module MUI.Core.TableBody where

import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_tbody)
import Unsafe.Coerce (unsafeCoerce)

type TableBodyProps a =
  ( children :: Array JSX
  , classes :: TableBodyClassKey
  , component :: ReactComponent { | a }
  )

foreign import data TableBodyClassKey :: Type
foreign import data TableBodyPropsPartial :: Type

type TableBodyClassKeyOptions = ( root :: String )

tableBodyClassKey :: ∀ options options_
  . Union options options_ TableBodyClassKeyOptions
  => Record options
  -> TableBodyClassKey
tableBodyClassKey = unsafeCoerce

tableBodyPropsPartial :: ∀ props props_
  . Union props props_ (TableBodyProps Props_tbody)
  => Record props 
  -> TableBodyPropsPartial
tableBodyPropsPartial = unsafeCoerce

tableBody :: ∀ props props_
  . Union props props_ (TableBodyProps Props_tbody)
  => Record props 
  -> JSX
tableBody = element _TableBody


foreign import  _TableBody :: ∀ a. ReactComponent a

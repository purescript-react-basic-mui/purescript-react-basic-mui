module MUI.Core.TableBody where

import React.Basic (JSX, ReactComponent, element)
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)

type TableBodyProps =
  ( children :: Array JSX
  , classes :: TableBodyClassKey
  , component :: String
  )

foreign import data TableBodyClassKey :: Type

type TableBodyClassKeyOptions = ( root :: String )

tableBodyClassKey 
  :: ∀ options options_
  . Union options options_ TableBodyClassKeyOptions
  => Record options
  -> TableBodyClassKey
tableBodyClassKey = unsafeCoerce

tableBody
  :: ∀ props props_
  . Union props props_ TableBodyProps
  => Record props 
  -> JSX
tableBody = element _TableBody


foreign import  _TableBody :: ∀ a. ReactComponent a

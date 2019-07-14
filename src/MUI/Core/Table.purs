module MUI.Core.Table where

import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import Unsafe.Coerce (unsafeCoerce)

type TableProps =
  ( children :: Array JSX
  , classes :: TableClassKey
  , component :: String
  , padding :: String
  , size :: String
  )

foreign import data TableClassKey :: Type

type TableClassKeyOptions = ( root :: String )

tableClassKey 
  :: ∀ options options_
  . Union options options_ TableClassKeyOptions
  => Record options
  -> TableClassKey
tableClassKey = unsafeCoerce

table
  :: ∀ props props_
  . Union props props_ TableProps
  => Record props 
  -> JSX
table = element _Table

foreign import  _Table :: ∀ a. ReactComponent a
module MUI.Core.TableRow where

import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import Unsafe.Coerce (unsafeCoerce)

type TableRowProps =
  ( children :: Array JSX
  , classes :: TableRowClassKey
  , component :: String
  , hover :: Boolean
  , selected :: Boolean
  )

foreign import data TableRowClassKey :: Type

type TableRowClassKeyOptions = 
  ( root :: String
  , selected :: String
  , hover :: String
  , head :: String
  , footer :: String
  )

tableRowClassKey 
  :: ∀ options options_
  . Union options options_ TableRowClassKeyOptions
  => Record options
  -> TableRowClassKey
tableRowClassKey = unsafeCoerce

tableRow
  :: ∀ props props_
  . Union props props_ TableRowProps
  => Record props 
  -> JSX
tableRow = element _TableRow

foreign import  _TableRow :: ∀ a. ReactComponent a

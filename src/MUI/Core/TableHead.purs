module MUI.Core.TableHead where

import Data.Maybe (Maybe)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import Unsafe.Coerce (unsafeCoerce)

type TableHeadProps =
  ( children :: Array JSX
  , classes :: TableHeadClassKey
  , component :: Maybe String
  )

foreign import data TableHeadClassKey :: Type

type TableHeadClassKeyOptions = ( root :: String )

tableHeadClassKey 
  :: ∀ options options_
  . Union options options_ TableHeadClassKeyOptions
  => Record options
  -> TableHeadClassKey
tableHeadClassKey = unsafeCoerce

tableHead
  :: ∀ props props_
  . Union props props_ TableHeadProps
  => Record props 
  -> JSX
tableHead = element _TableHead

foreign import  _TableHead :: ∀ a. ReactComponent a

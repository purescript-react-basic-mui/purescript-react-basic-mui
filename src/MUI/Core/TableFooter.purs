module MUI.Core.TableFooter where

import React.Basic (JSX, ReactComponent, element)
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)

type TableFooterProps =
  ( children :: Array JSX
  , classes :: TableFooterClassKey
  , component :: String
  )

foreign import data TableFooterClassKey :: Type

type TableFooterClassKeyOptions = ( root :: String )

tableFooterClassKey 
  :: ∀ options options_
  . Union options options_ TableFooterClassKeyOptions
  => Record options
  -> TableFooterClassKey
tableFooterClassKey = unsafeCoerce

tableFooter
  :: ∀ props props_
  . Union props props_ TableFooterProps
  => Record props 
  -> JSX
tableFooter = element _TableFooter

foreign import  _TableFooter :: ∀ a. ReactComponent a

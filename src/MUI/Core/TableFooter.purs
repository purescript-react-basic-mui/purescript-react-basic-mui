module MUI.Core.TableFooter where

import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_tfoot)
import Unsafe.Coerce (unsafeCoerce)

type TableFooterProps componentProps =
  ( children :: Array JSX
  , classes :: TableFooterClassKey
  , component :: ReactComponent { | componentProps } 
  | componentProps
  )

foreign import data TableFooterClassKey :: Type
foreign import data TableFooterPropsPartial :: Type

type TableFooterClassKeyOptions = ( root :: String )

tableFooterClassKey :: ∀ options options_
  .  Union options options_ TableFooterClassKeyOptions
  => Record options
  -> TableFooterClassKey
tableFooterClassKey = unsafeCoerce


tableFooterPropsPartial :: ∀ props props_
  .  Union props props_ (TableFooterProps Props_tfoot)
  => Record props 
  -> TableFooterPropsPartial
tableFooterPropsPartial = unsafeCoerce

tableFooter_component :: ∀ componentProps props props_
  .  Union props props_ (TableFooterProps componentProps)
  => Record props 
  -> JSX
tableFooter_component = element _TableFooter

tableFooter :: ∀ props props_
  .  Union props props_ (TableFooterProps Props_tfoot)
  => Record props 
  -> JSX
tableFooter = element _TableFooter

foreign import  _TableFooter :: ∀ a. ReactComponent a

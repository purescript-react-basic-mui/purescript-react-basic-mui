module MUI.Core.List where

import MUI.Core (JSS)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_div)
import Unsafe.Coerce (unsafeCoerce)

type ListProps componentProps =
  ( children :: Array JSX
  , classes :: ListClassKey
  , component :: ReactComponent { | componentProps }
  , dense :: Boolean
  , disablePadding :: Boolean
  , subheader :: JSX
  | componentProps 
  )

foreign import data ListClassKey :: Type
foreign import data ListClassKeyJSS :: Type
foreign import data ListPropsPartial :: Type

type ListClassKeyOptionsJSS = ListClassKeyOptionsR JSS
type ListClassKeyOptions = ListClassKeyOptionsR String
type ListClassKeyOptionsR a =
  ( root :: a
  , padding :: a
  , dense :: a
  , subheader :: a
  )

listClassKey :: ∀ options options_
  . Union options options_ ListClassKeyOptions
  => Record options
  -> ListClassKey
listClassKey = unsafeCoerce

listClassKeyJSS :: ∀ options options_
  . Union options options_ ListClassKeyOptionsJSS
  => Record options
  -> ListClassKeyJSS
listClassKeyJSS = unsafeCoerce

listPropsPartial_component :: ∀ componentProps props props_
  . Union props props_ (ListProps componentProps)
  => Record props 
  -> ListPropsPartial
listPropsPartial_component = unsafeCoerce

listPropsPartial :: ∀ props props_
  . Union props props_ (ListProps Props_div)
  => Record props 
  -> ListPropsPartial
listPropsPartial = unsafeCoerce

list_component :: ∀ componentProps props props_
  . Union props props_ (ListProps componentProps)
  => Record props 
  -> JSX
list_component = element _List

list :: ∀ props props_
  . Union props props_ (ListProps Props_div)
  => Record props 
  -> JSX
list = element _List

foreign import _List :: ∀ a. ReactComponent a
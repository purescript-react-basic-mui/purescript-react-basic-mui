module MUI.Core.List where

import React.Basic (JSX, ReactComponent, element)
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)

type ListProps =
  ( children :: Array JSX
  , classes :: ListClassKey
  , component :: String
  , dense :: Boolean
  , disablePadding :: Boolean
  , subheader :: JSX
  )

foreign import data ListClassKey :: Type
foreign import data ListPropsPartial :: Type

type ListClassKeyOptions =
  ( root :: String
  , padding :: String
  , dense :: String
  , subheader :: String
  )

listClassKey :: ∀ options options_
  . Union options options_ ListClassKeyOptions
  => Record options
  -> ListClassKey
listClassKey = unsafeCoerce

listPropsPartial :: ∀ props props_
  . Union props props_ ListProps
  => Record props 
  -> ListPropsPartial
listPropsPartial = unsafeCoerce


list :: ∀ props props_
  . Union props props_ ListProps
  => Record props 
  -> JSX
list = element _List

foreign import _List :: ∀ a. ReactComponent a
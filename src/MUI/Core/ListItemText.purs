module MUI.Core.ListItemText where

import MUI.Core (JSS)
import MUI.Core.Typography (TypographyProps)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_div)
import Unsafe.Coerce (unsafeCoerce)

type ListItemTextProps componentProps =
  ( children :: Array JSX
  , classes :: ListItemTextClassKey
  , disableTypography :: Boolean
  , inset :: Boolean
  , primary :: JSX
  , primaryTypographyProps :: TypographyProps
  , secondary :: JSX
  , secondaryTypographyProps :: TypographyProps
  | componentProps
  )

foreign import data ListItemTextClassKey :: Type
foreign import data ListItemTextClassKeyJSS :: Type
foreign import data ListItemTextPropsPartial :: Type

type ListItemTextClassKeyOptionsJSS = ListItemTextClassKeyOptionsR JSS
type ListItemTextClassKeyOptions = ListItemTextClassKeyOptionsR String
type ListItemTextClassKeyOptionsR a =
  ( root :: a
  , multiline :: a
  , dense :: a
  , inset :: a
  , primary :: a
  , secondary :: a
  )

listItemTextClassKey :: ∀ options options_
  . Union options options_ ListItemTextClassKeyOptions
  => Record options
  -> ListItemTextClassKey
listItemTextClassKey = unsafeCoerce

listItemTextClassKeyJSS :: ∀ options options_
  . Union options options_ ListItemTextClassKeyOptionsJSS
  => Record options
  -> ListItemTextClassKeyJSS
listItemTextClassKeyJSS = unsafeCoerce


listItemTextPropsPartial :: ∀ props props_
  . Union props props_ (ListItemTextProps Props_div)
  => Record props 
  -> ListItemTextPropsPartial 
listItemTextPropsPartial = unsafeCoerce

listItemText :: ∀ props props_
  . Union props props_ (ListItemTextProps Props_div)
  => Record props 
  -> JSX
listItemText = element _ListItemText

foreign import _ListItemText :: ∀ a. ReactComponent a

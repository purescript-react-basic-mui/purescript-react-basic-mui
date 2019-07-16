module MUI.Core.ListItemText where

import MUI.Core.Typography (TypographyPropsPartial)
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
  , primaryTypographyProps :: TypographyPropsPartial
  , secondary :: JSX
  , secondaryTypographyProps :: TypographyPropsPartial
  | componentProps
  )

foreign import data ListItemTextClassKey :: Type
foreign import data ListItemTextPropsPartial :: Type

type ListItemTextClassKeyOptions =
  ( root :: String
  , multiline :: String
  , dense :: String
  , inset :: String
  , primary :: String
  , secondary :: String
  )

listItemTextClassKey :: ∀ options options_
  . Union options options_ ListItemTextClassKeyOptions
  => Record options
  -> ListItemTextClassKey
listItemTextClassKey = unsafeCoerce

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
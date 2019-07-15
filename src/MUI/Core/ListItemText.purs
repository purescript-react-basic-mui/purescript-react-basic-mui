module MUI.Core.ListItemText where

import MUI.Core.Typography (TypographyProps)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import Unsafe.Coerce (unsafeCoerce)

type ListItemTextProps =
  ( children :: Array JSX
  , classes :: ListItemTextClassKey
  , disableTypography :: Boolean
  , inset :: Boolean
  , primary :: JSX
  , primaryTypographyProps :: { | TypographyProps }
  , secondary :: JSX
  , secondaryTypographyProps :: { | TypographyProps }
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
  . Union props props_ ListItemTextProps
  => Record props 
  -> ListItemTextPropsPartial 
listItemTextPropsPartial = unsafeCoerce

listItemText :: ∀ props props_
  . Union props props_ ListItemTextProps
  => Record props 
  -> JSX
listItemText = element _ListItemText

foreign import _ListItemText :: ∀ a. ReactComponent a
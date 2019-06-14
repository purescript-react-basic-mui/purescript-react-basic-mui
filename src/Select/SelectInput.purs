module React.Basic.MUI.Select.SelectInput where 

import Prelude
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic (element, ReactComponent, JSX)
import React.Basic.MUI.Menu (MenuProps)
import React.Basic.Events (EventHandler)

type SelectInputProps_required optional =
  ( autoWidth :: Boolean
  , multiple :: Boolean
  , native :: Boolean
  , value :: Foreign
  | optional )

type SelectInputProps_optional =
  ( autoFocus :: Boolean
  , disabled :: Boolean
  , "IconComponent" :: JSX
  , inputRef :: Foreign
  , "MenuProps" :: Foreign
  , name :: String
  , onBlur :: EventHandler
  , onChange :: Foreign
  , onClose :: Foreign
  , onFocus :: EventHandler
  , onOpen :: Foreign
  , open :: Boolean
  , readOnly :: Boolean
  , renderValue :: Foreign
  , "SelectDisplayProps" :: Foreign
  , tabIndex :: Number
  , variant :: Foreign
  )

foreign import data SelectInputProps :: Type 

selectInputProps
  :: ∀ attrs attrs_
   . Union attrs attrs_ (SelectInputProps_optional)
  => Record (SelectInputProps_required attrs)
  -> SelectInputProps
selectInputProps = unsafeCoerce

selectInput
  :: ∀ attrs attrs_
   . Union attrs attrs_ (SelectInputProps_optional)
  => Record (SelectInputProps_required attrs)
  -> JSX
selectInput = element _SelectInput
foreign import _SelectInput :: forall a. ReactComponent a 
module React.Basic.MUI.ListItemSecondaryAction where 

import Prelude
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic.DOM.Internal (CSS)
import React.Basic (element, ReactComponent, JSX)

type ListItemSecondaryActionProps_optional =
  ( classes :: Foreign
  , innerRef :: Foreign
  , className :: String
  , style :: CSS
  , ref :: Foreign
  )

foreign import data ListItemSecondaryActionProps :: Type 

listItemSecondaryActionProps
  :: ∀ attrs attrs_
   . Union attrs attrs_ (ListItemSecondaryActionProps_optional)
  => Record (attrs)
  -> ListItemSecondaryActionProps
listItemSecondaryActionProps = unsafeCoerce

type ListItemSecondaryActionClassKey = String

listItemSecondaryAction
  :: ∀ attrs attrs_
   . Union attrs attrs_ (ListItemSecondaryActionProps_optional)
  => Record (attrs)
  -> JSX
listItemSecondaryAction = element _ListItemSecondaryAction
foreign import _ListItemSecondaryAction :: forall a. ReactComponent a 
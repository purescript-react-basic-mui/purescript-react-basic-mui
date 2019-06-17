module React.Basic.MUI.Core.ListItemAvatar where 

import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic.DOM.Internal (CSS)
import React.Basic (element, ReactComponent, JSX)

type ListItemAvatarProps_optional =
  ( classes :: Foreign
  , innerRef :: Foreign
  , className :: String
  , style :: CSS
  , ref :: Foreign
  )

foreign import data ListItemAvatarProps :: Type 

listItemAvatarProps
  :: ∀ attrs attrs_
   . Union attrs attrs_ (ListItemAvatarProps_optional )
  => Record (attrs)
  -> ListItemAvatarProps
listItemAvatarProps = unsafeCoerce

type ListItemAvatarClassKey = Foreign

listItemAvatar
  :: ∀ attrs attrs_
   . Union attrs attrs_ (ListItemAvatarProps_optional )
  => Record (attrs)
  -> JSX
listItemAvatar = element _ListItemAvatar
foreign import _ListItemAvatar :: forall a. ReactComponent a 
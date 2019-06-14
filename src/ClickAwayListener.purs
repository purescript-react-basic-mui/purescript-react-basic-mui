module React.Basic.MUI.ClickAwayListener where 

import Prelude
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic (element, ReactComponent, JSX)

type ClickAwayListenerProps_required optional =
  ( children :: JSX
  , onClickAway :: Foreign
  | optional )

type ClickAwayListenerProps_optional =
  ( mouseEvent :: Foreign
  , touchEvent :: Foreign
  )

foreign import data ClickAwayListenerProps :: Type 

clickAwayListenerProps
  :: ∀ attrs attrs_
   . Union attrs attrs_ (ClickAwayListenerProps_optional)
  => Record (ClickAwayListenerProps_required attrs)
  -> ClickAwayListenerProps
clickAwayListenerProps = unsafeCoerce

clickAwayListener
  :: ∀ attrs attrs_
   . Union attrs attrs_ (ClickAwayListenerProps_optional)
  => Record (ClickAwayListenerProps_required attrs)
  -> JSX
clickAwayListener = element _ClickAwayListener
foreign import _ClickAwayListener :: forall a. ReactComponent a 
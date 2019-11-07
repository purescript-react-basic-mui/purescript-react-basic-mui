module MUI.Core.ClickAwayListener where

import Effect (Effect) as Effect
import MUI.Core (shallowEq) as MUI.Core
import Prelude
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_div) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce

foreign import data TouchEvent :: Type

touchEvent :: { "false" :: TouchEvent, onTouchEnd :: TouchEvent, onTouchStart :: TouchEvent }
touchEvent = { false: Unsafe.Coerce.unsafeCoerce false, onTouchEnd: Unsafe.Coerce.unsafeCoerce "onTouchEnd", onTouchStart: Unsafe.Coerce.unsafeCoerce "onTouchStart" }

foreign import data MouseEvent :: Type

mouseEvent :: { "false" :: MouseEvent, onClick :: MouseEvent, onMouseDown :: MouseEvent, onMouseUp :: MouseEvent }
mouseEvent = { false: Unsafe.Coerce.unsafeCoerce false, onClick: Unsafe.Coerce.unsafeCoerce "onClick", onMouseDown: Unsafe.Coerce.unsafeCoerce "onMouseDown", onMouseUp: Unsafe.Coerce.unsafeCoerce "onMouseUp" }

instance eqMouseEvent :: Eq MouseEvent where
  eq = MUI.Core.shallowEq

instance eqTouchEvent :: Eq TouchEvent where
  eq = MUI.Core.shallowEq

type ClickAwayListenerPropsOptions componentProps = ( children :: React.Basic.JSX, mouseEvent :: MouseEvent, onClickAway :: Effect.Effect Unit, touchEvent :: TouchEvent | componentProps )

foreign import data ClickAwayListenerProps :: Type

foreign import _ClickAwayListener :: ∀ a. React.Basic.ReactComponent a

clickAwayListener :: ∀ required given. Prim.Row.Union given required (ClickAwayListenerPropsOptions React.Basic.DOM.Props_div) => Record given -> React.Basic.JSX
clickAwayListener = React.Basic.element _ClickAwayListener

clickAwayListener_component :: ∀ required given componentProps. Prim.Row.Union given required (ClickAwayListenerPropsOptions componentProps) => Record given -> React.Basic.JSX
clickAwayListener_component = React.Basic.element _ClickAwayListener

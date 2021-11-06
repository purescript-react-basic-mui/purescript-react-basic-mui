{- This module was autogenerated. Please don't edit. -}
module MUI.Core.Dialog where

import Effect (Effect) as Effect
import Foreign (Foreign) as Foreign
import MUI.Core (JSS, class Nub')
import MUI.Core.Paper (PaperProps) as MUI.Core.Paper
import MUI.Core.Styles (Theme, withStyles) as MUI.Core.Styles
import MUI.React.Basic (element) as MUI.React.Basic
import MUI.React.TransitionGroup (Timeout) as MUI.React.TransitionGroup
import Prelude
import Prim.Row (class Union) as Prim.Row
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_div) as React.Basic.DOM
import React.Basic.Events (EventHandler) as React.Basic.Events
import Unsafe.Coerce (unsafeCoerce)
import Unsafe.Reference (unsafeRefEq)

foreign import data MaxWidth :: Type

maxWidth ::
  { "false" :: MaxWidth
  , lg :: MaxWidth
  , md :: MaxWidth
  , sm :: MaxWidth
  , xl :: MaxWidth
  , xs :: MaxWidth
  }
maxWidth = { "false": unsafeCoerce false, lg: unsafeCoerce "lg", md: unsafeCoerce "md", sm: unsafeCoerce "sm", xl: unsafeCoerce "xl", xs: unsafeCoerce "xs" }

foreign import data Scroll :: Type

scroll ::
  { body :: Scroll
  , paper :: Scroll
  }
scroll = { body: unsafeCoerce "body", paper: unsafeCoerce "paper" }

instance eqScroll :: Eq Scroll where
  eq = unsafeRefEq

instance eqMaxWidth :: Eq MaxWidth where
  eq = unsafeRefEq

type DialogClassesGenericRow a
  = ( container :: a
    , paper :: a
    , paperFullScreen :: a
    , paperFullWidth :: a
    , paperScrollBody :: a
    , paperScrollPaper :: a
    , paperWidthFalse :: a
    , paperWidthLg :: a
    , paperWidthMd :: a
    , paperWidthSm :: a
    , paperWidthXl :: a
    , paperWidthXs :: a
    , root :: a
    , scrollBody :: a
    , scrollPaper :: a
    )

type DialogClassesKey
  = DialogClassesGenericRow String

type DialogClassesJSS
  = DialogClassesGenericRow JSS

type DialogOptPropsRow (r :: # Type)
  = ( "PaperComponent" :: Foreign.Foreign
    , "PaperProps" :: MUI.Core.Paper.PaperProps
    , "TransitionComponent" :: Foreign.Foreign
    , "TransitionProps" :: Foreign.Foreign
    , "aria-describedby" :: String
    , "aria-labelledby" :: String
    , children :: Array JSX
    , classes :: { | DialogClassesKey }
    , disableBackdropClick :: Boolean
    , disableEscapeKeyDown :: Boolean
    , fullScreen :: Boolean
    , fullWidth :: Boolean
    , maxWidth :: MaxWidth
    , onBackdropClick :: React.Basic.Events.EventHandler
    , onClose :: React.Basic.Events.EventHandler
    , onEnter :: React.Basic.Events.EventHandler
    , onEntered :: React.Basic.Events.EventHandler
    , onEntering :: React.Basic.Events.EventHandler
    , onEscapeKeyDown :: React.Basic.Events.EventHandler
    , onExit :: React.Basic.Events.EventHandler
    , onExited :: React.Basic.Events.EventHandler
    , onExiting :: React.Basic.Events.EventHandler
    , ref :: Foreign.Foreign
    , scroll :: Scroll
    , transitionDuration :: MUI.React.TransitionGroup.Timeout
    | r
    )

type DialogReqPropsRow (r :: # Type)
  = ( open :: Boolean
    | r
    )

type DialogPropsRow (r :: # Type)
  = DialogOptPropsRow (DialogReqPropsRow r)

foreign import _UnsafeDialog :: forall componentProps. ReactComponent { | DialogPropsRow componentProps }

_Dialog ::
  forall given optionalGiven optionalMissing props required.
  Nub' (DialogReqPropsRow ()) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (DialogPropsRow React.Basic.DOM.Props_div) props =>
  Prim.Row.Union given optionalMissing props =>
  ReactComponent { | given }
_Dialog = unsafeCoerce _UnsafeDialog

dialog ::
  forall given optionalGiven optionalMissing props required.
  Nub' (DialogReqPropsRow ()) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (DialogPropsRow React.Basic.DOM.Props_div) props =>
  Prim.Row.Union given optionalMissing props =>
  { | given } -> JSX
dialog ps = element _Dialog ps

dialog' :: DialogProps -> JSX
dialog' = MUI.React.Basic.element _Dialog'

_Dialog' :: ReactComponent DialogProps
_Dialog' = unsafeCoerce _UnsafeDialog

dialogWithStyles ::
  forall jss_ jss.
  Prim.Row.Union jss jss_ DialogClassesJSS =>
  (MUI.Core.Styles.Theme -> { | jss }) -> Effect.Effect (DialogProps -> JSX)
dialogWithStyles style = render
  where
  withStyles' :: ReactComponent DialogProps -> Effect.Effect (ReactComponent DialogProps)
  withStyles' = MUI.Core.Styles.withStyles (unsafeCoerce style)

  styledComponent = withStyles' _Dialog'

  render = map MUI.React.Basic.element styledComponent

foreign import data DialogProps :: Type

props ::
  forall given optionalGiven optionalMissing props required.
  Nub' (DialogReqPropsRow ()) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (DialogPropsRow React.Basic.DOM.Props_div) props =>
  Prim.Row.Union given optionalMissing props =>
  { | given } -> DialogProps
props = unsafeCoerce

{- This module was autogenerated. Please don't edit. -}
module MUI.Core.DialogContent where

import Effect (Effect) as Effect
import Foreign (Foreign) as Foreign
import MUI.Core (JSS, class Nub')
import MUI.Core.Styles (Theme, withStyles) as MUI.Core.Styles
import MUI.React.Basic (element) as MUI.React.Basic
import Prelude
import Prim.Row (class Union) as Prim.Row
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_div) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce)

type DialogContentClassesGenericRow a
  = ( dividers :: a
    , root :: a
    )

type DialogContentClassesKey
  = DialogContentClassesGenericRow String

type DialogContentClassesJSS
  = DialogContentClassesGenericRow JSS

type DialogContentOptPropsRow (r :: # Type)
  = ( children :: Array JSX
    , classes :: { | DialogContentClassesKey }
    , dividers :: Boolean
    , ref :: Foreign.Foreign
    | r
    )

type DialogContentReqPropsRow (r :: # Type)
  = r

type DialogContentPropsRow (r :: # Type)
  = DialogContentOptPropsRow (DialogContentReqPropsRow r)

foreign import _UnsafeDialogContent :: forall componentProps. ReactComponent { | DialogContentPropsRow componentProps }

_DialogContent ::
  forall given optionalGiven optionalMissing props required.
  Nub' (DialogContentReqPropsRow ()) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (DialogContentPropsRow React.Basic.DOM.Props_div) props =>
  Prim.Row.Union given optionalMissing props =>
  ReactComponent { | given }
_DialogContent = unsafeCoerce _UnsafeDialogContent

dialogContent ::
  forall given optionalGiven optionalMissing props required.
  Nub' (DialogContentReqPropsRow ()) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (DialogContentPropsRow React.Basic.DOM.Props_div) props =>
  Prim.Row.Union given optionalMissing props =>
  { | given } -> JSX
dialogContent ps = element _DialogContent ps

dialogContent' :: DialogContentProps -> JSX
dialogContent' = MUI.React.Basic.element _DialogContent'

_DialogContent' :: ReactComponent DialogContentProps
_DialogContent' = unsafeCoerce _UnsafeDialogContent

dialogContentWithStyles ::
  forall jss_ jss.
  Prim.Row.Union jss jss_ DialogContentClassesJSS =>
  (MUI.Core.Styles.Theme -> { | jss }) -> Effect.Effect (DialogContentProps -> JSX)
dialogContentWithStyles style = render
  where
  withStyles' :: ReactComponent DialogContentProps -> Effect.Effect (ReactComponent DialogContentProps)
  withStyles' = MUI.Core.Styles.withStyles (unsafeCoerce style)

  styledComponent = withStyles' _DialogContent'

  render = map MUI.React.Basic.element styledComponent

foreign import data DialogContentProps :: Type

props ::
  forall given optionalGiven optionalMissing props required.
  Nub' (DialogContentReqPropsRow ()) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (DialogContentPropsRow React.Basic.DOM.Props_div) props =>
  Prim.Row.Union given optionalMissing props =>
  { | given } -> DialogContentProps
props = unsafeCoerce

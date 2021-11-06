{- This module was autogenerated. Please don't edit. -}
module MUI.Core.DialogContentText where

import Effect (Effect) as Effect
import Foreign (Foreign) as Foreign
import MUI.Core (JSS, class Nub')
import MUI.Core.Styles (Theme, withStyles) as MUI.Core.Styles
import MUI.Core.Typography (TypographyPropsRow, TypographyReqPropsRow) as MUI.Core.Typography
import MUI.React.Basic (element) as MUI.React.Basic
import Prelude
import Prim.Row (class Union) as Prim.Row
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_p) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce)

type DialogContentTextClassesGenericRow a
  = ( root :: a
    )

type DialogContentTextClassesKey
  = DialogContentTextClassesGenericRow String

type DialogContentTextClassesJSS
  = DialogContentTextClassesGenericRow JSS

type DialogContentTextOptPropsRow (r :: # Type)
  = ( children :: Array JSX
    , classes :: { | DialogContentTextClassesKey }
    , ref :: Foreign.Foreign
    | r
    )

type DialogContentTextReqPropsRow (r :: # Type)
  = r

type DialogContentTextPropsRow (r :: # Type)
  = DialogContentTextOptPropsRow (DialogContentTextReqPropsRow r)

foreign import _UnsafeDialogContentText :: forall componentProps. ReactComponent { | DialogContentTextPropsRow componentProps }

_DialogContentText ::
  forall given optionalGiven optionalMissing props required.
  Nub' (DialogContentTextReqPropsRow (MUI.Core.Typography.TypographyReqPropsRow ())) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (DialogContentTextPropsRow (MUI.Core.Typography.TypographyPropsRow React.Basic.DOM.Props_p)) props =>
  Prim.Row.Union given optionalMissing props =>
  ReactComponent { | given }
_DialogContentText = unsafeCoerce _UnsafeDialogContentText

dialogContentText ::
  forall given optionalGiven optionalMissing props required.
  Nub' (DialogContentTextReqPropsRow (MUI.Core.Typography.TypographyReqPropsRow ())) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (DialogContentTextPropsRow (MUI.Core.Typography.TypographyPropsRow React.Basic.DOM.Props_p)) props =>
  Prim.Row.Union given optionalMissing props =>
  { | given } -> JSX
dialogContentText ps = element _DialogContentText ps

dialogContentText' :: DialogContentTextProps -> JSX
dialogContentText' = MUI.React.Basic.element _DialogContentText'

_DialogContentText' :: ReactComponent DialogContentTextProps
_DialogContentText' = unsafeCoerce _UnsafeDialogContentText

dialogContentTextWithStyles ::
  forall jss_ jss.
  Prim.Row.Union jss jss_ DialogContentTextClassesJSS =>
  (MUI.Core.Styles.Theme -> { | jss }) -> Effect.Effect (DialogContentTextProps -> JSX)
dialogContentTextWithStyles style = render
  where
  withStyles' :: ReactComponent DialogContentTextProps -> Effect.Effect (ReactComponent DialogContentTextProps)
  withStyles' = MUI.Core.Styles.withStyles (unsafeCoerce style)

  styledComponent = withStyles' _DialogContentText'

  render = map MUI.React.Basic.element styledComponent

foreign import data DialogContentTextProps :: Type

props ::
  forall given optionalGiven optionalMissing props required.
  Nub' (DialogContentTextReqPropsRow (MUI.Core.Typography.TypographyReqPropsRow ())) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (DialogContentTextPropsRow (MUI.Core.Typography.TypographyPropsRow React.Basic.DOM.Props_p)) props =>
  Prim.Row.Union given optionalMissing props =>
  { | given } -> DialogContentTextProps
props = unsafeCoerce

{- This module was autogenerated. Please don't edit. -}
module MUI.Core.DialogTitle where

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

type DialogTitleClassesGenericRow a
  = ( root :: a
    )

type DialogTitleClassesKey
  = DialogTitleClassesGenericRow String

type DialogTitleClassesJSS
  = DialogTitleClassesGenericRow JSS

type DialogTitleOptPropsRow (r :: # Type)
  = ( children :: Array JSX
    , classes :: { | DialogTitleClassesKey }
    , disableTypography :: Boolean
    , ref :: Foreign.Foreign
    | r
    )

type DialogTitleReqPropsRow (r :: # Type)
  = r

type DialogTitlePropsRow (r :: # Type)
  = DialogTitleOptPropsRow (DialogTitleReqPropsRow r)

foreign import _UnsafeDialogTitle :: forall componentProps. ReactComponent { | DialogTitlePropsRow componentProps }

_DialogTitle ::
  forall given optionalGiven optionalMissing props required.
  Nub' (DialogTitleReqPropsRow ()) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (DialogTitlePropsRow React.Basic.DOM.Props_div) props =>
  Prim.Row.Union given optionalMissing props =>
  ReactComponent { | given }
_DialogTitle = unsafeCoerce _UnsafeDialogTitle

dialogTitle ::
  forall given optionalGiven optionalMissing props required.
  Nub' (DialogTitleReqPropsRow ()) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (DialogTitlePropsRow React.Basic.DOM.Props_div) props =>
  Prim.Row.Union given optionalMissing props =>
  { | given } -> JSX
dialogTitle ps = element _DialogTitle ps

dialogTitle' :: DialogTitleProps -> JSX
dialogTitle' = MUI.React.Basic.element _DialogTitle'

_DialogTitle' :: ReactComponent DialogTitleProps
_DialogTitle' = unsafeCoerce _UnsafeDialogTitle

dialogTitleWithStyles ::
  forall jss_ jss.
  Prim.Row.Union jss jss_ DialogTitleClassesJSS =>
  (MUI.Core.Styles.Theme -> { | jss }) -> Effect.Effect (DialogTitleProps -> JSX)
dialogTitleWithStyles style = render
  where
  withStyles' :: ReactComponent DialogTitleProps -> Effect.Effect (ReactComponent DialogTitleProps)
  withStyles' = MUI.Core.Styles.withStyles (unsafeCoerce style)

  styledComponent = withStyles' _DialogTitle'

  render = map MUI.React.Basic.element styledComponent

foreign import data DialogTitleProps :: Type

props ::
  forall given optionalGiven optionalMissing props required.
  Nub' (DialogTitleReqPropsRow ()) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (DialogTitlePropsRow React.Basic.DOM.Props_div) props =>
  Prim.Row.Union given optionalMissing props =>
  { | given } -> DialogTitleProps
props = unsafeCoerce

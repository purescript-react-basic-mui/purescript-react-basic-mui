{- This module was autogenerated. Please don't edit. -}
module MUI.Core.SnackbarContent where

import Effect (Effect) as Effect
import Foreign (Foreign) as Foreign
import MUI.Core (JSS, class Nub')
import MUI.Core.Paper (PaperPropsRow, PaperReqPropsRow) as MUI.Core.Paper
import MUI.Core.Styles (Theme, withStyles) as MUI.Core.Styles
import MUI.React.Basic (element) as MUI.React.Basic
import Prelude
import Prim.Row (class Union) as Prim.Row
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_div) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce)

type SnackbarContentClassesGenericRow a
  = ( action :: a
    , message :: a
    , root :: a
    )

type SnackbarContentClassesKey
  = SnackbarContentClassesGenericRow String

type SnackbarContentClassesJSS
  = SnackbarContentClassesGenericRow JSS

type SnackbarContentOptPropsRow (r :: # Type)
  = ( action :: JSX
    , classes :: { | SnackbarContentClassesKey }
    , message :: JSX
    , ref :: Foreign.Foreign
    , role :: String
    | r
    )

type SnackbarContentReqPropsRow (r :: # Type)
  = r

type SnackbarContentPropsRow (r :: # Type)
  = SnackbarContentOptPropsRow (SnackbarContentReqPropsRow r)

foreign import _UnsafeSnackbarContent :: forall componentProps. ReactComponent { | SnackbarContentPropsRow componentProps }

_SnackbarContent ::
  forall given optionalGiven optionalMissing props required.
  Nub' (SnackbarContentReqPropsRow (MUI.Core.Paper.PaperReqPropsRow ())) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (SnackbarContentPropsRow (MUI.Core.Paper.PaperPropsRow React.Basic.DOM.Props_div)) props =>
  Prim.Row.Union given optionalMissing props =>
  ReactComponent { | given }
_SnackbarContent = unsafeCoerce _UnsafeSnackbarContent

snackbarContent ::
  forall given optionalGiven optionalMissing props required.
  Nub' (SnackbarContentReqPropsRow (MUI.Core.Paper.PaperReqPropsRow ())) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (SnackbarContentPropsRow (MUI.Core.Paper.PaperPropsRow React.Basic.DOM.Props_div)) props =>
  Prim.Row.Union given optionalMissing props =>
  { | given } -> JSX
snackbarContent ps = element _SnackbarContent ps

snackbarContent' :: SnackbarContentProps -> JSX
snackbarContent' = MUI.React.Basic.element _SnackbarContent'

_SnackbarContent' :: ReactComponent SnackbarContentProps
_SnackbarContent' = unsafeCoerce _UnsafeSnackbarContent

snackbarContentWithStyles ::
  forall jss_ jss.
  Prim.Row.Union jss jss_ SnackbarContentClassesJSS =>
  (MUI.Core.Styles.Theme -> { | jss }) -> Effect.Effect (SnackbarContentProps -> JSX)
snackbarContentWithStyles style = render
  where
  withStyles' :: ReactComponent SnackbarContentProps -> Effect.Effect (ReactComponent SnackbarContentProps)
  withStyles' = MUI.Core.Styles.withStyles (unsafeCoerce style)

  styledComponent = withStyles' _SnackbarContent'

  render = map MUI.React.Basic.element styledComponent

foreign import data SnackbarContentProps :: Type

props ::
  forall given optionalGiven optionalMissing props required.
  Nub' (SnackbarContentReqPropsRow (MUI.Core.Paper.PaperReqPropsRow ())) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (SnackbarContentPropsRow (MUI.Core.Paper.PaperPropsRow React.Basic.DOM.Props_div)) props =>
  Prim.Row.Union given optionalMissing props =>
  { | given } -> SnackbarContentProps
props = unsafeCoerce

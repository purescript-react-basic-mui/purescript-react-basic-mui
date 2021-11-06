{- This module was autogenerated. Please don't edit. -}
module MUI.Core.ListItemText where

import Effect (Effect) as Effect
import MUI.Core (JSS, class Nub')
import MUI.Core.Styles (Theme, withStyles) as MUI.Core.Styles
import MUI.Core.Typography (TypographyProps) as MUI.Core.Typography
import MUI.React.Basic (element) as MUI.React.Basic
import Prelude
import Prim.Row (class Union) as Prim.Row
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_div) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce)

type ListItemTextClassesGenericRow a
  = ( dense :: a
    , inset :: a
    , multiline :: a
    , primary :: a
    , root :: a
    , secondary :: a
    )

type ListItemTextClassesKey
  = ListItemTextClassesGenericRow String

type ListItemTextClassesJSS
  = ListItemTextClassesGenericRow JSS

type ListItemTextOptPropsRow (r :: # Type)
  = ( classes :: { | ListItemTextClassesKey }
    , disableTypography :: Boolean
    , inset :: Boolean
    , primary :: JSX
    , primaryTypographyProps :: MUI.Core.Typography.TypographyProps
    , secondary :: JSX
    , secondaryTypographyProps :: MUI.Core.Typography.TypographyProps
    | r
    )

type ListItemTextReqPropsRow (r :: # Type)
  = r

type ListItemTextPropsRow (r :: # Type)
  = ListItemTextOptPropsRow (ListItemTextReqPropsRow r)

foreign import _UnsafeListItemText :: forall componentProps. ReactComponent { | ListItemTextPropsRow componentProps }

_ListItemText ::
  forall given optionalGiven optionalMissing props required.
  Nub' (ListItemTextReqPropsRow ()) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (ListItemTextPropsRow React.Basic.DOM.Props_div) props =>
  Prim.Row.Union given optionalMissing props =>
  ReactComponent { | given }
_ListItemText = unsafeCoerce _UnsafeListItemText

listItemText ::
  forall given optionalGiven optionalMissing props required.
  Nub' (ListItemTextReqPropsRow ()) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (ListItemTextPropsRow React.Basic.DOM.Props_div) props =>
  Prim.Row.Union given optionalMissing props =>
  { | given } -> JSX
listItemText ps = element _ListItemText ps

listItemText' :: ListItemTextProps -> JSX
listItemText' = MUI.React.Basic.element _ListItemText'

_ListItemText' :: ReactComponent ListItemTextProps
_ListItemText' = unsafeCoerce _UnsafeListItemText

listItemTextWithStyles ::
  forall jss_ jss.
  Prim.Row.Union jss jss_ ListItemTextClassesJSS =>
  (MUI.Core.Styles.Theme -> { | jss }) -> Effect.Effect (ListItemTextProps -> JSX)
listItemTextWithStyles style = render
  where
  withStyles' :: ReactComponent ListItemTextProps -> Effect.Effect (ReactComponent ListItemTextProps)
  withStyles' = MUI.Core.Styles.withStyles (unsafeCoerce style)

  styledComponent = withStyles' _ListItemText'

  render = map MUI.React.Basic.element styledComponent

foreign import data ListItemTextProps :: Type

props ::
  forall given optionalGiven optionalMissing props required.
  Nub' (ListItemTextReqPropsRow ()) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (ListItemTextPropsRow React.Basic.DOM.Props_div) props =>
  Prim.Row.Union given optionalMissing props =>
  { | given } -> ListItemTextProps
props = unsafeCoerce

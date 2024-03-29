{- This module was autogenerated. Please don't edit. -}
module MUI.Core.TableSortLabel where

import Effect (Effect) as Effect
import Foreign (Foreign) as Foreign
import MUI.Core (JSS, class Nub')
import MUI.Core.ButtonBase (ButtonBasePropsRow, ButtonBaseReqPropsRow) as MUI.Core.ButtonBase
import MUI.Core.Styles (Theme, withStyles) as MUI.Core.Styles
import MUI.React.Basic (element) as MUI.React.Basic
import Prelude
import Prim.Row (class Union) as Prim.Row
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_button) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce)
import Unsafe.Reference (unsafeRefEq)

foreign import data Direction :: Type

direction ::
  { asc :: Direction
  , desc :: Direction
  }
direction = { asc: unsafeCoerce "asc", desc: unsafeCoerce "desc" }

instance eqDirection :: Eq Direction where
  eq = unsafeRefEq

type TableSortLabelClassesGenericRow a
  = ( active :: a
    , icon :: a
    , iconDirectionAsc :: a
    , iconDirectionDesc :: a
    , root :: a
    )

type TableSortLabelClassesKey
  = TableSortLabelClassesGenericRow String

type TableSortLabelClassesJSS
  = TableSortLabelClassesGenericRow JSS

type TableSortLabelOptPropsRow (r :: # Type)
  = ( "IconComponent" :: Foreign.Foreign
    , active :: Boolean
    , children :: Array JSX
    , classes :: { | TableSortLabelClassesKey }
    , direction :: Direction
    , hideSortIcon :: Boolean
    , ref :: Foreign.Foreign
    | r
    )

type TableSortLabelReqPropsRow (r :: # Type)
  = r

type TableSortLabelPropsRow (r :: # Type)
  = TableSortLabelOptPropsRow (TableSortLabelReqPropsRow r)

foreign import _UnsafeTableSortLabel :: forall componentProps. ReactComponent { | TableSortLabelPropsRow componentProps }

_TableSortLabel ::
  forall given optionalGiven optionalMissing props required.
  Nub' (TableSortLabelReqPropsRow (MUI.Core.ButtonBase.ButtonBaseReqPropsRow ())) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (TableSortLabelPropsRow (MUI.Core.ButtonBase.ButtonBasePropsRow React.Basic.DOM.Props_button)) props =>
  Prim.Row.Union given optionalMissing props =>
  ReactComponent { | given }
_TableSortLabel = unsafeCoerce _UnsafeTableSortLabel

tableSortLabel ::
  forall given optionalGiven optionalMissing props required.
  Nub' (TableSortLabelReqPropsRow (MUI.Core.ButtonBase.ButtonBaseReqPropsRow ())) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (TableSortLabelPropsRow (MUI.Core.ButtonBase.ButtonBasePropsRow React.Basic.DOM.Props_button)) props =>
  Prim.Row.Union given optionalMissing props =>
  { | given } -> JSX
tableSortLabel ps = element _TableSortLabel ps

tableSortLabel' :: TableSortLabelProps -> JSX
tableSortLabel' = MUI.React.Basic.element _TableSortLabel'

_TableSortLabel' :: ReactComponent TableSortLabelProps
_TableSortLabel' = unsafeCoerce _UnsafeTableSortLabel

tableSortLabelWithStyles ::
  forall jss_ jss.
  Prim.Row.Union jss jss_ TableSortLabelClassesJSS =>
  (MUI.Core.Styles.Theme -> { | jss }) -> Effect.Effect (TableSortLabelProps -> JSX)
tableSortLabelWithStyles style = render
  where
  withStyles' :: ReactComponent TableSortLabelProps -> Effect.Effect (ReactComponent TableSortLabelProps)
  withStyles' = MUI.Core.Styles.withStyles (unsafeCoerce style)

  styledComponent = withStyles' _TableSortLabel'

  render = map MUI.React.Basic.element styledComponent

foreign import data TableSortLabelProps :: Type

props ::
  forall given optionalGiven optionalMissing props required.
  Nub' (TableSortLabelReqPropsRow (MUI.Core.ButtonBase.ButtonBaseReqPropsRow ())) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (TableSortLabelPropsRow (MUI.Core.ButtonBase.ButtonBasePropsRow React.Basic.DOM.Props_button)) props =>
  Prim.Row.Union given optionalMissing props =>
  { | given } -> TableSortLabelProps
props = unsafeCoerce

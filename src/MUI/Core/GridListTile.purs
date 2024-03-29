{- This module was autogenerated. Please don't edit. -}
module MUI.Core.GridListTile where

import Effect (Effect) as Effect
import MUI.Core (JSS, class Nub')
import MUI.Core.Styles (Theme, withStyles) as MUI.Core.Styles
import MUI.React.Basic (element) as MUI.React.Basic
import Prelude
import Prim.Row (class Union) as Prim.Row
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_ul) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce)

type GridListTileClassesGenericRow a
  = ( imgFullHeight :: a
    , imgFullWidth :: a
    , root :: a
    , tile :: a
    )

type GridListTileClassesKey
  = GridListTileClassesGenericRow String

type GridListTileClassesJSS
  = GridListTileClassesGenericRow JSS

type GridListTileOptPropsRow (r :: # Type)
  = ( children :: Array JSX
    , classes :: { | GridListTileClassesKey }
    , cols :: Number
    , rows :: Number
    | r
    )

type GridListTileReqPropsRow (r :: # Type)
  = r

type GridListTilePropsRow (r :: # Type)
  = GridListTileOptPropsRow (GridListTileReqPropsRow r)

foreign import _UnsafeGridListTile :: forall componentProps. ReactComponent { | GridListTilePropsRow componentProps }

_GridListTile ::
  forall given optionalGiven optionalMissing props required.
  Nub' (GridListTileReqPropsRow ()) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (GridListTilePropsRow React.Basic.DOM.Props_ul) props =>
  Prim.Row.Union given optionalMissing props =>
  ReactComponent { | given }
_GridListTile = unsafeCoerce _UnsafeGridListTile

gridListTile ::
  forall given optionalGiven optionalMissing props required.
  Nub' (GridListTileReqPropsRow ()) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (GridListTilePropsRow React.Basic.DOM.Props_ul) props =>
  Prim.Row.Union given optionalMissing props =>
  { | given } -> JSX
gridListTile ps = element _GridListTile ps

gridListTile' :: GridListTileProps -> JSX
gridListTile' = MUI.React.Basic.element _GridListTile'

_GridListTile' :: ReactComponent GridListTileProps
_GridListTile' = unsafeCoerce _UnsafeGridListTile

gridListTileWithStyles ::
  forall jss_ jss.
  Prim.Row.Union jss jss_ GridListTileClassesJSS =>
  (MUI.Core.Styles.Theme -> { | jss }) -> Effect.Effect (GridListTileProps -> JSX)
gridListTileWithStyles style = render
  where
  withStyles' :: ReactComponent GridListTileProps -> Effect.Effect (ReactComponent GridListTileProps)
  withStyles' = MUI.Core.Styles.withStyles (unsafeCoerce style)

  styledComponent = withStyles' _GridListTile'

  render = map MUI.React.Basic.element styledComponent

foreign import data GridListTileProps :: Type

props ::
  forall given optionalGiven optionalMissing props required.
  Nub' (GridListTileReqPropsRow ()) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (GridListTilePropsRow React.Basic.DOM.Props_ul) props =>
  Prim.Row.Union given optionalMissing props =>
  { | given } -> GridListTileProps
props = unsafeCoerce

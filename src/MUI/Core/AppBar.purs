{- This module was autogenerated. Please don't edit. -}
module MUI.Core.AppBar where

import Effect (Effect) as Effect
import MUI.Core (JSS, class Nub')
import MUI.Core.Paper (PaperPropsRow, PaperReqPropsRow) as MUI.Core.Paper
import MUI.Core.Styles (Theme, withStyles) as MUI.Core.Styles
import MUI.React.Basic (element) as MUI.React.Basic
import Prelude
import Prim.Row (class Union) as Prim.Row
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_div) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce)
import Unsafe.Reference (unsafeRefEq)

foreign import data AriaHaspopup :: Type

ariaHaspopup ::
  { dialog :: AriaHaspopup
  , "false" :: AriaHaspopup
  , grid :: AriaHaspopup
  , listbox :: AriaHaspopup
  , menu :: AriaHaspopup
  , tree :: AriaHaspopup
  , "true" :: AriaHaspopup
  }
ariaHaspopup = { dialog: unsafeCoerce "dialog", "false": unsafeCoerce "false", grid: unsafeCoerce "grid", listbox: unsafeCoerce "listbox", menu: unsafeCoerce "menu", tree: unsafeCoerce "tree", "true": unsafeCoerce "true" }

foreign import data Color :: Type

color ::
  { default :: Color
  , inherit :: Color
  , primary :: Color
  , secondary :: Color
  , transparent :: Color
  }
color = { default: unsafeCoerce "default", inherit: unsafeCoerce "inherit", primary: unsafeCoerce "primary", secondary: unsafeCoerce "secondary", transparent: unsafeCoerce "transparent" }

foreign import data Position :: Type

position ::
  { absolute :: Position
  , fixed :: Position
  , relative :: Position
  , static :: Position
  , sticky :: Position
  }
position = { absolute: unsafeCoerce "absolute", fixed: unsafeCoerce "fixed", relative: unsafeCoerce "relative", static: unsafeCoerce "static", sticky: unsafeCoerce "sticky" }

instance eqPosition :: Eq Position where
  eq = unsafeRefEq

instance eqColor :: Eq Color where
  eq = unsafeRefEq

instance eqAriaHaspopup :: Eq AriaHaspopup where
  eq = unsafeRefEq

type AppBarClassesGenericRow a
  = ( colorDefault :: a
    , colorPrimary :: a
    , colorSecondary :: a
    , positionAbsolute :: a
    , positionFixed :: a
    , positionRelative :: a
    , positionStatic :: a
    , positionSticky :: a
    , root :: a
    )

type AppBarClassesKey
  = AppBarClassesGenericRow String

type AppBarClassesJSS
  = AppBarClassesGenericRow JSS

type AppBarOptPropsRow (r :: # Type)
  = ( "aria-controls" :: String
    , "aria-haspopup" :: AriaHaspopup
    , children :: Array JSX
    , classes :: { | AppBarClassesKey }
    , color :: Color
    , elevation :: Number
    , position :: Position
    | r
    )

type AppBarReqPropsRow (r :: # Type)
  = r

type AppBarPropsRow (r :: # Type)
  = AppBarOptPropsRow (AppBarReqPropsRow r)

foreign import _UnsafeAppBar :: forall componentProps. ReactComponent { | AppBarPropsRow componentProps }

_AppBar ::
  forall given optionalGiven optionalMissing props required.
  Nub' (AppBarReqPropsRow (MUI.Core.Paper.PaperReqPropsRow ())) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (AppBarPropsRow (MUI.Core.Paper.PaperPropsRow React.Basic.DOM.Props_div)) props =>
  Prim.Row.Union given optionalMissing props =>
  ReactComponent { | given }
_AppBar = unsafeCoerce _UnsafeAppBar

appBar ::
  forall given optionalGiven optionalMissing props required.
  Nub' (AppBarReqPropsRow (MUI.Core.Paper.PaperReqPropsRow ())) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (AppBarPropsRow (MUI.Core.Paper.PaperPropsRow React.Basic.DOM.Props_div)) props =>
  Prim.Row.Union given optionalMissing props =>
  { | given } -> JSX
appBar ps = element _AppBar ps

appBar' :: AppBarProps -> JSX
appBar' = MUI.React.Basic.element _AppBar'

_AppBar' :: ReactComponent AppBarProps
_AppBar' = unsafeCoerce _UnsafeAppBar

appBarWithStyles ::
  forall jss_ jss.
  Prim.Row.Union jss jss_ AppBarClassesJSS =>
  (MUI.Core.Styles.Theme -> { | jss }) -> Effect.Effect (AppBarProps -> JSX)
appBarWithStyles style = render
  where
  withStyles' :: ReactComponent AppBarProps -> Effect.Effect (ReactComponent AppBarProps)
  withStyles' = MUI.Core.Styles.withStyles (unsafeCoerce style)

  styledComponent = withStyles' _AppBar'

  render = map MUI.React.Basic.element styledComponent

foreign import data AppBarProps :: Type

props ::
  forall given optionalGiven optionalMissing props required.
  Nub' (AppBarReqPropsRow (MUI.Core.Paper.PaperReqPropsRow ())) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (AppBarPropsRow (MUI.Core.Paper.PaperPropsRow React.Basic.DOM.Props_div)) props =>
  Prim.Row.Union given optionalMissing props =>
  { | given } -> AppBarProps
props = unsafeCoerce

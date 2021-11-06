{- This module was autogenerated. Please don't edit. -}
module MUI.Core.Fab where

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

foreign import data Color :: Type

color ::
  { default :: Color
  , inherit :: Color
  , primary :: Color
  , secondary :: Color
  }
color = { default: unsafeCoerce "default", inherit: unsafeCoerce "inherit", primary: unsafeCoerce "primary", secondary: unsafeCoerce "secondary" }

foreign import data Size :: Type

size ::
  { large :: Size
  , medium :: Size
  , small :: Size
  }
size = { large: unsafeCoerce "large", medium: unsafeCoerce "medium", small: unsafeCoerce "small" }

foreign import data Variant :: Type

variant ::
  { extended :: Variant
  , round :: Variant
  }
variant = { extended: unsafeCoerce "extended", round: unsafeCoerce "round" }

instance eqVariant :: Eq Variant where
  eq = unsafeRefEq

instance eqSize :: Eq Size where
  eq = unsafeRefEq

instance eqColor :: Eq Color where
  eq = unsafeRefEq

type FabClassesGenericRow a
  = ( colorInherit :: a
    , disabled :: a
    , extended :: a
    , focusVisible :: a
    , label :: a
    , primary :: a
    , root :: a
    , secondary :: a
    , sizeMedium :: a
    , sizeSmall :: a
    )

type FabClassesKey
  = FabClassesGenericRow String

type FabClassesJSS
  = FabClassesGenericRow JSS

type FabOptPropsRow (r :: # Type)
  = ( children :: Array JSX
    , classes :: { | FabClassesKey }
    , color :: Color
    , component :: Foreign.Foreign
    , disableFocusRipple :: Boolean
    , disableRipple :: Boolean
    , disabled :: Boolean
    , href :: String
    , ref :: Foreign.Foreign
    , size :: Size
    , variant :: Variant
    | r
    )

type FabReqPropsRow (r :: # Type)
  = r

type FabPropsRow (r :: # Type)
  = FabOptPropsRow (FabReqPropsRow r)

foreign import _UnsafeFab :: forall componentProps. ReactComponent { | FabPropsRow componentProps }

_Fab ::
  forall given optionalGiven optionalMissing props required.
  Nub' (FabReqPropsRow (MUI.Core.ButtonBase.ButtonBaseReqPropsRow ())) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (FabPropsRow (MUI.Core.ButtonBase.ButtonBasePropsRow React.Basic.DOM.Props_button)) props =>
  Prim.Row.Union given optionalMissing props =>
  ReactComponent { | given }
_Fab = unsafeCoerce _UnsafeFab

fab ::
  forall given optionalGiven optionalMissing props required.
  Nub' (FabReqPropsRow (MUI.Core.ButtonBase.ButtonBaseReqPropsRow ())) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (FabPropsRow (MUI.Core.ButtonBase.ButtonBasePropsRow React.Basic.DOM.Props_button)) props =>
  Prim.Row.Union given optionalMissing props =>
  { | given } -> JSX
fab ps = element _Fab ps

fab' :: FabProps -> JSX
fab' = MUI.React.Basic.element _Fab'

_Fab' :: ReactComponent FabProps
_Fab' = unsafeCoerce _UnsafeFab

fabWithStyles ::
  forall jss_ jss.
  Prim.Row.Union jss jss_ FabClassesJSS =>
  (MUI.Core.Styles.Theme -> { | jss }) -> Effect.Effect (FabProps -> JSX)
fabWithStyles style = render
  where
  withStyles' :: ReactComponent FabProps -> Effect.Effect (ReactComponent FabProps)
  withStyles' = MUI.Core.Styles.withStyles (unsafeCoerce style)

  styledComponent = withStyles' _Fab'

  render = map MUI.React.Basic.element styledComponent

foreign import data FabProps :: Type

props ::
  forall given optionalGiven optionalMissing props required.
  Nub' (FabReqPropsRow (MUI.Core.ButtonBase.ButtonBaseReqPropsRow ())) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (FabPropsRow (MUI.Core.ButtonBase.ButtonBasePropsRow React.Basic.DOM.Props_button)) props =>
  Prim.Row.Union given optionalMissing props =>
  { | given } -> FabProps
props = unsafeCoerce

{- This module was autogenerated. Please don't edit. -}
module MUI.Core.Button where

import Effect (Effect) as Effect
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
  { contained :: Variant
  , outlined :: Variant
  , text :: Variant
  }
variant = { contained: unsafeCoerce "contained", outlined: unsafeCoerce "outlined", text: unsafeCoerce "text" }

instance eqVariant :: Eq Variant where
  eq = unsafeRefEq

instance eqSize :: Eq Size where
  eq = unsafeRefEq

instance eqColor :: Eq Color where
  eq = unsafeRefEq

type ButtonClassesGenericRow a
  = ( colorInherit :: a
    , contained :: a
    , containedPrimary :: a
    , containedSecondary :: a
    , containedSizeLarge :: a
    , containedSizeSmall :: a
    , disableElevation :: a
    , disabled :: a
    , endIcon :: a
    , focusVisible :: a
    , fullWidth :: a
    , iconSizeLarge :: a
    , iconSizeMedium :: a
    , iconSizeSmall :: a
    , label :: a
    , outlined :: a
    , outlinedPrimary :: a
    , outlinedSecondary :: a
    , outlinedSizeLarge :: a
    , outlinedSizeSmall :: a
    , root :: a
    , sizeLarge :: a
    , sizeSmall :: a
    , startIcon :: a
    , text :: a
    , textPrimary :: a
    , textSecondary :: a
    , textSizeLarge :: a
    , textSizeSmall :: a
    )

type ButtonClassesKey
  = ButtonClassesGenericRow String

type ButtonClassesJSS
  = ButtonClassesGenericRow JSS

type ButtonOptPropsRow (r :: # Type)
  = ( classes :: { | ButtonClassesKey }
    , color :: Color
    , disableFocusRipple :: Boolean
    , disableRipple :: Boolean
    , disabled :: Boolean
    , endIcon :: JSX
    , fullWidth :: Boolean
    , href :: String
    , size :: Size
    , startIcon :: JSX
    , variant :: Variant
    | r
    )

type ButtonReqPropsRow (r :: # Type)
  = r

type ButtonPropsRow (r :: # Type)
  = ButtonOptPropsRow (ButtonReqPropsRow r)

foreign import _UnsafeButton :: forall componentProps. ReactComponent { | ButtonPropsRow componentProps }

_Button ::
  forall given optionalGiven optionalMissing props required.
  Nub' (ButtonReqPropsRow (MUI.Core.ButtonBase.ButtonBaseReqPropsRow ())) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (ButtonPropsRow (MUI.Core.ButtonBase.ButtonBasePropsRow React.Basic.DOM.Props_button)) props =>
  Prim.Row.Union given optionalMissing props =>
  ReactComponent { | given }
_Button = unsafeCoerce _UnsafeButton

button ::
  forall given optionalGiven optionalMissing props required.
  Nub' (ButtonReqPropsRow (MUI.Core.ButtonBase.ButtonBaseReqPropsRow ())) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (ButtonPropsRow (MUI.Core.ButtonBase.ButtonBasePropsRow React.Basic.DOM.Props_button)) props =>
  Prim.Row.Union given optionalMissing props =>
  { | given } -> JSX
button ps = element _Button ps

button' :: ButtonProps -> JSX
button' = MUI.React.Basic.element _Button'

_Button' :: ReactComponent ButtonProps
_Button' = unsafeCoerce _UnsafeButton

buttonWithStyles ::
  forall jss_ jss.
  Prim.Row.Union jss jss_ ButtonClassesJSS =>
  (MUI.Core.Styles.Theme -> { | jss }) -> Effect.Effect (ButtonProps -> JSX)
buttonWithStyles style = render
  where
  withStyles' :: ReactComponent ButtonProps -> Effect.Effect (ReactComponent ButtonProps)
  withStyles' = MUI.Core.Styles.withStyles (unsafeCoerce style)

  styledComponent = withStyles' _Button'

  render = map MUI.React.Basic.element styledComponent

foreign import data ButtonProps :: Type

props ::
  forall given optionalGiven optionalMissing props required.
  Nub' (ButtonReqPropsRow (MUI.Core.ButtonBase.ButtonBaseReqPropsRow ())) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (ButtonPropsRow (MUI.Core.ButtonBase.ButtonBasePropsRow React.Basic.DOM.Props_button)) props =>
  Prim.Row.Union given optionalMissing props =>
  { | given } -> ButtonProps
props = unsafeCoerce

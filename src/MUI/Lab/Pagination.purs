{- This module was autogenerated. Please don't edit. -}
module MUI.Lab.Pagination where

import Effect (Effect) as Effect
import MUI.Core (JSS, class Nub')
import MUI.Core.Styles (Theme, withStyles) as MUI.Core.Styles
import MUI.React.Basic (element) as MUI.React.Basic
import Prelude
import Prim.Row (class Union) as Prim.Row
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_div) as React.Basic.DOM
import React.Basic.Events (EventHandler) as React.Basic.Events
import Unsafe.Coerce (unsafeCoerce)
import Unsafe.Reference (unsafeRefEq)

foreign import data Color :: Type

color ::
  { primary :: Color
  , secondary :: Color
  , standard :: Color
  }
color = { primary: unsafeCoerce "primary", secondary: unsafeCoerce "secondary", standard: unsafeCoerce "standard" }

foreign import data Shape :: Type

shape ::
  { round :: Shape
  , rounded :: Shape
  }
shape = { round: unsafeCoerce "round", rounded: unsafeCoerce "rounded" }

foreign import data Size :: Type

size ::
  { large :: Size
  , medium :: Size
  , small :: Size
  }
size = { large: unsafeCoerce "large", medium: unsafeCoerce "medium", small: unsafeCoerce "small" }

foreign import data Variant :: Type

variant ::
  { outlined :: Variant
  , text :: Variant
  }
variant = { outlined: unsafeCoerce "outlined", text: unsafeCoerce "text" }

instance eqVariant :: Eq Variant where
  eq = unsafeRefEq

instance eqSize :: Eq Size where
  eq = unsafeRefEq

instance eqShape :: Eq Shape where
  eq = unsafeRefEq

instance eqColor :: Eq Color where
  eq = unsafeRefEq

type PaginationClassesGenericRow a
  = ( root :: a
    , ul :: a
    )

type PaginationClassesKey
  = PaginationClassesGenericRow String

type PaginationClassesJSS
  = PaginationClassesGenericRow JSS

type PaginationOptPropsRow (r :: # Type)
  = ( boundryCount :: Int
    , classes :: { | PaginationClassesKey }
    , color :: Color
    , count :: Int
    , defaultPage :: Number
    , disabled :: Boolean
    , hideNextButton :: Boolean
    , hidePrevButton :: Boolean
    , onChange :: React.Basic.Events.EventHandler
    , page :: Number
    , shape :: Shape
    , showFirstButton :: Boolean
    , showLastButton :: Boolean
    , siblingCount :: Int
    , size :: Size
    , variant :: Variant
    | r
    )

type PaginationReqPropsRow (r :: # Type)
  = r

type PaginationPropsRow (r :: # Type)
  = PaginationOptPropsRow (PaginationReqPropsRow r)

foreign import _UnsafePagination :: forall componentProps. ReactComponent { | PaginationPropsRow componentProps }

_Pagination ::
  forall given optionalGiven optionalMissing props required.
  Nub' (PaginationReqPropsRow ()) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (PaginationPropsRow React.Basic.DOM.Props_div) props =>
  Prim.Row.Union given optionalMissing props =>
  ReactComponent { | given }
_Pagination = unsafeCoerce _UnsafePagination

pagination ::
  forall given optionalGiven optionalMissing props required.
  Nub' (PaginationReqPropsRow ()) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (PaginationPropsRow React.Basic.DOM.Props_div) props =>
  Prim.Row.Union given optionalMissing props =>
  { | given } -> JSX
pagination ps = element _Pagination ps

pagination' :: PaginationProps -> JSX
pagination' = MUI.React.Basic.element _Pagination'

_Pagination' :: ReactComponent PaginationProps
_Pagination' = unsafeCoerce _UnsafePagination

paginationWithStyles ::
  forall jss_ jss.
  Prim.Row.Union jss jss_ PaginationClassesJSS =>
  (MUI.Core.Styles.Theme -> { | jss }) -> Effect.Effect (PaginationProps -> JSX)
paginationWithStyles style = render
  where
  withStyles' :: ReactComponent PaginationProps -> Effect.Effect (ReactComponent PaginationProps)
  withStyles' = MUI.Core.Styles.withStyles (unsafeCoerce style)

  styledComponent = withStyles' _Pagination'

  render = map MUI.React.Basic.element styledComponent

foreign import data PaginationProps :: Type

props ::
  forall given optionalGiven optionalMissing props required.
  Nub' (PaginationReqPropsRow ()) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (PaginationPropsRow React.Basic.DOM.Props_div) props =>
  Prim.Row.Union given optionalMissing props =>
  { | given } -> PaginationProps
props = unsafeCoerce

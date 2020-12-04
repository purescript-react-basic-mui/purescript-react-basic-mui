module MUI.Icons.Types
  ( icon
  , iconWithStyles
  , Icon
  , IconProps
  , props
  ) where

import Prelude
import Effect (Effect)
import MUI.Core (class Nub', JSS)
import MUI.Core.Styles.Types (Theme)
import MUI.Core.Styles.WithStyles (withStyles)
import MUI.Core.SvgIcon (SvgIconPropsRow, SvgIconReqPropsRow, SvgIconClassesJSS)
import MUI.React.Basic (element) as MUI.React.Basic
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM.Internal (SharedSVGProps)
import React.Basic.DOM.SVG (Props_svg)
import Unsafe.Coerce (unsafeCoerce)

newtype Icon
  = Icon (forall props. ReactComponent props)

toComponent ∷ ∀ props. Icon → ReactComponent props
toComponent (Icon c) = c

icon ::
  ∀ given optionalMissing optionalPresent props required.
  Nub' (SvgIconReqPropsRow ()) required =>
  Union required optionalPresent given =>
  Nub' (SvgIconPropsRow (SharedSVGProps Props_svg)) props =>
  Union given optionalMissing props =>
  Icon -> { | given } -> JSX
icon (Icon i) = element i

props ∷
  ∀ given optionalMissing optionalPresent props required.
  Nub' (SvgIconReqPropsRow ()) required =>
  Union required optionalPresent given =>
  Nub' (SvgIconPropsRow (SharedSVGProps Props_svg)) props =>
  Union given optionalMissing props =>
  { | given } -> IconProps
props = unsafeCoerce

-- | We need this opaque props type
-- | so our effectful `iconWithStyles`
-- | constructor execution is not blocked by
-- | type classes dicts resolution.
foreign import data IconProps ∷ Type

toComponent' ∷ Icon → ReactComponent IconProps
toComponent' (Icon c) = unsafeCoerce c

iconWithStyles ::
  ∀ jss jss_.
  Union jss jss_ SvgIconClassesJSS =>
  (Theme -> Record jss) ->
  Icon ->
  Effect (IconProps -> JSX)
iconWithStyles style i =
  let
    style' :: Theme -> JSS
    style' = unsafeCoerce style

    styledIcon ∷ Effect (ReactComponent IconProps)
    styledIcon = withStyles style' (toComponent' i)
  in
    do
      i' <- styledIcon
      pure $ MUI.React.Basic.element i'

module MUI.Icons.Types
  ( icon
  , iconWithStyles
  , Icon
  ) where

import MUI.Core (class Nub')
import MUI.Core.Styles.Types (Theme)
import MUI.Core.Styles.WithStyles (withStyles)
import MUI.Core.SvgIcon (SvgIconPropsRow, SvgIconReqPropsRow, SvgIconClassesJSS)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM.Internal (SharedSVGProps)
import React.Basic.DOM.SVG (Props_svg)
import Unsafe.Coerce (unsafeCoerce)

newtype Icon
  = Icon (forall props. ReactComponent props)

toComponent :: forall props. Icon -> ReactComponent props
toComponent (Icon c) = c

icon ::
  forall given optionalMissing optionalPresent props required.
  Nub' (SvgIconReqPropsRow ()) required =>
  Union required optionalPresent given =>
  Nub' (SvgIconPropsRow (SharedSVGProps Props_svg)) props =>
  Union given optionalMissing props =>
  Icon -> { | given } -> JSX
icon (Icon i) = element i

iconWithStyles ::
  forall given jss jss_ optionalMissing optionalPresent props required.
  Nub' (SvgIconReqPropsRow ()) required =>
  Union required optionalPresent given =>
  Nub' (SvgIconPropsRow (SharedSVGProps Props_svg)) props =>
  Union given optionalMissing props =>
  Union jss jss_ SvgIconClassesJSS =>
  (Theme -> Record jss) ->
  Icon ->
  Record given ->
  JSX
iconWithStyles style (Icon i) = element (unsafeCoerce withStyles style i)

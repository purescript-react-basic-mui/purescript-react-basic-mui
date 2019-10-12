module MUI.Icon
  ( icon
  , iconWithStyles
  , Icon
  )
  where

import MUI.Core.Styles (withStyles)
import MUI.Core.Styles.CreateMuiTheme (Theme)
import MUI.Core.SvgIcon (SvgIconProps, SvgIconClassKeyOptionsJSS)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_svg)
import Unsafe.Coerce (unsafeCoerce)

newtype Icon (name ∷ Symbol) = Icon (∀ props. ReactComponent props)

icon
  :: ∀ given icon required
  . Union given required (SvgIconProps Props_svg)
  ⇒ Icon icon
  → Record given
  → JSX
icon (Icon i) = element i

iconWithStyles ∷ ∀ jss jss_ required given icon
  . Union given required (SvgIconProps Props_svg)
  ⇒ Union jss jss_ SvgIconClassKeyOptionsJSS
  ⇒ Icon icon
  → (Theme → Record jss)
  → Record given
  → JSX
iconWithStyles (Icon i) style = element (unsafeCoerce withStyles style i)


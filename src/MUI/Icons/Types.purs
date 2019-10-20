module MUI.Icons.Types
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

newtype Icon = Icon (∀ props. ReactComponent props)

toComponent :: ∀ props. Icon -> ReactComponent props
toComponent (Icon c) = c

icon
  :: ∀ given required
  . Union given required (SvgIconProps Props_svg)
  => Icon
  -> Record given
  -> JSX
icon (Icon i) = element i

iconWithStyles :: ∀ jss jss_ required given
  . Union given required (SvgIconProps Props_svg)
  => Union jss jss_ SvgIconClassKeyOptionsJSS
  => Icon
  -> (Theme -> Record jss)
  -> Record given
  -> JSX
iconWithStyles (Icon i) style = element (unsafeCoerce withStyles style i)


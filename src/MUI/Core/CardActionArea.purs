module MUI.Core.CardActionArea where

import MUI.Core (JSS) as MUI.Core
import MUI.Core.ButtonBase (ButtonBasePropsOptions) as MUI.Core.ButtonBase
import MUI.Core.Styles.Types (Theme) as MUI.Core.Styles.Types
import MUI.Core.Styles.WithStyles (withStyles) as MUI.Core.Styles.WithStyles
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_button) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce

type CardActionAreaPropsOptions componentProps = ( children :: Array React.Basic.JSX, classes :: CardActionAreaClassKey | componentProps )

foreign import data CardActionAreaProps :: Type

foreign import data CardActionAreaPropsPartial :: Type

cardActionAreaPropsPartial :: ∀ options_ options. Prim.Row.Union options options_ (CardActionAreaPropsOptions (MUI.Core.ButtonBase.ButtonBasePropsOptions React.Basic.DOM.Props_button)) => Record options -> CardActionAreaPropsPartial
cardActionAreaPropsPartial = Unsafe.Coerce.unsafeCoerce

type CardActionAreaClassKeyGenericOptions a = ( focusHighlight :: a, focusVisible :: a, root :: a )

type CardActionAreaClassKeyOptions  = CardActionAreaClassKeyGenericOptions String

foreign import data CardActionAreaClassKey :: Type

cardActionAreaClassKey :: ∀ required given. Prim.Row.Union given required CardActionAreaClassKeyOptions => Record given -> CardActionAreaClassKey
cardActionAreaClassKey = Unsafe.Coerce.unsafeCoerce

type CardActionAreaClassKeyOptionsJSS  = CardActionAreaClassKeyGenericOptions MUI.Core.JSS

foreign import data CardActionAreaClassKeyJSS :: Type

cardActionAreaClassKeyJSS :: ∀ required given. Prim.Row.Union given required CardActionAreaClassKeyOptionsJSS => Record given -> CardActionAreaClassKeyJSS
cardActionAreaClassKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _CardActionArea :: ∀ a. React.Basic.ReactComponent a

cardActionArea :: ∀ required given. Prim.Row.Union given required (CardActionAreaPropsOptions (MUI.Core.ButtonBase.ButtonBasePropsOptions React.Basic.DOM.Props_button)) => Record given -> React.Basic.JSX
cardActionArea = React.Basic.element _CardActionArea

cardActionArea_component :: ∀ required given componentProps. Prim.Row.Union given required (CardActionAreaPropsOptions componentProps) => Record given -> React.Basic.JSX
cardActionArea_component = React.Basic.element _CardActionArea

cardActionAreaWithStyles :: ∀ required jss_ jss given. Prim.Row.Union given required (CardActionAreaPropsOptions (MUI.Core.ButtonBase.ButtonBasePropsOptions React.Basic.DOM.Props_button)) => Prim.Row.Union jss jss_ CardActionAreaClassKeyOptionsJSS => (MUI.Core.Styles.Types.Theme -> Record jss) -> Record given -> React.Basic.JSX
cardActionAreaWithStyles style = React.Basic.element (Unsafe.Coerce.unsafeCoerce MUI.Core.Styles.WithStyles.withStyles style _CardActionArea)
module MUI.Core.StepIcon where

import MUI.Core (JSS) as MUI.Core
import MUI.Core.Styles.Types (Theme) as MUI.Core.Styles.Types
import MUI.Core.Styles.WithStyles (withStyles) as MUI.Core.Styles.WithStyles
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_div) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce

type StepIconPropsOptions componentProps = ( active :: Boolean, children :: Array React.Basic.JSX, classes :: StepIconClassKey, completed :: Boolean, error :: Boolean, icon :: React.Basic.JSX | componentProps )

foreign import data StepIconProps :: Type

foreign import data StepIconPropsPartial :: Type

stepIconPropsPartial :: ∀ options_ options. Prim.Row.Union options options_ (StepIconPropsOptions React.Basic.DOM.Props_div) => Record options -> StepIconPropsPartial
stepIconPropsPartial = Unsafe.Coerce.unsafeCoerce

type StepIconClassKeyGenericOptions a = ( active :: a, completed :: a, error :: a, root :: a, text :: a )

type StepIconClassKeyOptions  = StepIconClassKeyGenericOptions String

foreign import data StepIconClassKey :: Type

stepIconClassKey :: ∀ required given. Prim.Row.Union given required StepIconClassKeyOptions => Record given -> StepIconClassKey
stepIconClassKey = Unsafe.Coerce.unsafeCoerce

type StepIconClassKeyOptionsJSS  = StepIconClassKeyGenericOptions MUI.Core.JSS

foreign import data StepIconClassKeyJSS :: Type

stepIconClassKeyJSS :: ∀ required given. Prim.Row.Union given required StepIconClassKeyOptionsJSS => Record given -> StepIconClassKeyJSS
stepIconClassKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _StepIcon :: ∀ a. React.Basic.ReactComponent a

stepIcon :: ∀ required given. Prim.Row.Union given required (StepIconPropsOptions React.Basic.DOM.Props_div) => Record given -> React.Basic.JSX
stepIcon = React.Basic.element _StepIcon

stepIcon_component :: ∀ required given componentProps. Prim.Row.Union given required (StepIconPropsOptions componentProps) => Record given -> React.Basic.JSX
stepIcon_component = React.Basic.element _StepIcon

stepIconWithStyles :: ∀ required jss_ jss given. Prim.Row.Union given required (StepIconPropsOptions React.Basic.DOM.Props_div) => Prim.Row.Union jss jss_ StepIconClassKeyOptionsJSS => (MUI.Core.Styles.Types.Theme -> Record jss) -> Record given -> React.Basic.JSX
stepIconWithStyles style = React.Basic.element (Unsafe.Coerce.unsafeCoerce MUI.Core.Styles.WithStyles.withStyles style _StepIcon)
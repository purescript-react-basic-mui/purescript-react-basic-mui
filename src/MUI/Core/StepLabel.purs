module MUI.Core.StepLabel where

import Foreign (Foreign) as Foreign
import MUI.Core (JSS) as MUI.Core
import MUI.Core.StepIcon (StepIconProps) as MUI.Core.StepIcon
import MUI.Core.Styles.Types (Theme) as MUI.Core.Styles.Types
import MUI.Core.Styles.WithStyles (withStyles) as MUI.Core.Styles.WithStyles
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_div) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce

type StepLabelPropsOptions componentProps = ( "StepIconComponent" :: Foreign.Foreign, "StepIconProps" :: MUI.Core.StepIcon.StepIconProps, children :: Array React.Basic.JSX, classes :: StepLabelClassKey, disabled :: Boolean, error :: Boolean, icon :: React.Basic.JSX, optional :: React.Basic.JSX | componentProps )

foreign import data StepLabelProps :: Type

foreign import data StepLabelPropsPartial :: Type

stepLabelPropsPartial :: ∀ options_ options. Prim.Row.Union options options_ (StepLabelPropsOptions React.Basic.DOM.Props_div) => Record options -> StepLabelPropsPartial
stepLabelPropsPartial = Unsafe.Coerce.unsafeCoerce

type StepLabelClassKeyGenericOptions a = ( active :: a, alternativeLabel :: a, completed :: a, disabled :: a, error :: a, horizontal :: a, iconContainer :: a, label :: a, labelContainer :: a, root :: a, vertical :: a )

type StepLabelClassKeyOptions  = StepLabelClassKeyGenericOptions String

foreign import data StepLabelClassKey :: Type

stepLabelClassKey :: ∀ required given. Prim.Row.Union given required StepLabelClassKeyOptions => Record given -> StepLabelClassKey
stepLabelClassKey = Unsafe.Coerce.unsafeCoerce

type StepLabelClassKeyOptionsJSS  = StepLabelClassKeyGenericOptions MUI.Core.JSS

foreign import data StepLabelClassKeyJSS :: Type

stepLabelClassKeyJSS :: ∀ required given. Prim.Row.Union given required StepLabelClassKeyOptionsJSS => Record given -> StepLabelClassKeyJSS
stepLabelClassKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _StepLabel :: ∀ a. React.Basic.ReactComponent a

stepLabel :: ∀ required given. Prim.Row.Union given required (StepLabelPropsOptions React.Basic.DOM.Props_div) => Record given -> React.Basic.JSX
stepLabel = React.Basic.element _StepLabel

stepLabel_component :: ∀ required given componentProps. Prim.Row.Union given required (StepLabelPropsOptions componentProps) => Record given -> React.Basic.JSX
stepLabel_component = React.Basic.element _StepLabel

stepLabelWithStyles :: ∀ required jss_ jss given. Prim.Row.Union given required (StepLabelPropsOptions React.Basic.DOM.Props_div) => Prim.Row.Union jss jss_ StepLabelClassKeyOptionsJSS => (MUI.Core.Styles.Types.Theme -> Record jss) -> Record given -> React.Basic.JSX
stepLabelWithStyles style = React.Basic.element (Unsafe.Coerce.unsafeCoerce MUI.Core.Styles.WithStyles.withStyles style _StepLabel)
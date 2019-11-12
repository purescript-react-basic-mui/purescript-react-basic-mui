module MUI.Core.FormControlLabel where

import Foreign (Foreign) as Foreign
import MUI.Core (JSS) as MUI.Core
import MUI.Core.Styles.Types (Theme) as MUI.Core.Styles.Types
import MUI.Core.Styles.WithStyles (withStyles) as MUI.Core.Styles.WithStyles
import Prelude
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_label) as React.Basic.DOM
import React.Basic.Events (EventHandler) as React.Basic.Events
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce
import Unsafe.Reference (unsafeRefEq) as Unsafe.Reference

foreign import data LabelPlacement :: Type

labelPlacement :: { bottom :: LabelPlacement, end :: LabelPlacement, start :: LabelPlacement, top :: LabelPlacement }
labelPlacement = { bottom: Unsafe.Coerce.unsafeCoerce "bottom", end: Unsafe.Coerce.unsafeCoerce "end", start: Unsafe.Coerce.unsafeCoerce "start", top: Unsafe.Coerce.unsafeCoerce "top" }

instance eqLabelPlacement :: Eq LabelPlacement where
  eq = Unsafe.Reference.unsafeRefEq

type FormControlLabelPropsOptions componentProps = ( checked :: Boolean, children :: Array React.Basic.JSX, classes :: FormControlLabelClassKey, control :: React.Basic.JSX, disabled :: Boolean, label :: React.Basic.JSX, labelPlacement :: LabelPlacement, name :: String, onChange :: React.Basic.Events.EventHandler, value :: Foreign.Foreign | componentProps )

foreign import data FormControlLabelProps :: Type

foreign import data FormControlLabelPropsPartial :: Type

formControlLabelPropsPartial :: ∀ options_ options. Prim.Row.Union options options_ (FormControlLabelPropsOptions React.Basic.DOM.Props_label) => Record options -> FormControlLabelPropsPartial
formControlLabelPropsPartial = Unsafe.Coerce.unsafeCoerce

type FormControlLabelClassKeyGenericOptions a = ( disabled :: a, label :: a, labelPlacementBottom :: a, labelPlacementStart :: a, labelPlacementTop :: a, root :: a )

type FormControlLabelClassKeyOptions  = FormControlLabelClassKeyGenericOptions String

foreign import data FormControlLabelClassKey :: Type

formControlLabelClassKey :: ∀ required given. Prim.Row.Union given required FormControlLabelClassKeyOptions => Record given -> FormControlLabelClassKey
formControlLabelClassKey = Unsafe.Coerce.unsafeCoerce

type FormControlLabelClassKeyOptionsJSS  = FormControlLabelClassKeyGenericOptions MUI.Core.JSS

foreign import data FormControlLabelClassKeyJSS :: Type

formControlLabelClassKeyJSS :: ∀ required given. Prim.Row.Union given required FormControlLabelClassKeyOptionsJSS => Record given -> FormControlLabelClassKeyJSS
formControlLabelClassKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _FormControlLabel :: ∀ a. React.Basic.ReactComponent a

formControlLabel :: ∀ required given. Prim.Row.Union given required (FormControlLabelPropsOptions React.Basic.DOM.Props_label) => Record given -> React.Basic.JSX
formControlLabel = React.Basic.element _FormControlLabel

formControlLabel_component :: ∀ required given componentProps. Prim.Row.Union given required (FormControlLabelPropsOptions componentProps) => Record given -> React.Basic.JSX
formControlLabel_component = React.Basic.element _FormControlLabel

formControlLabelWithStyles :: ∀ required jss_ jss given. Prim.Row.Union given required (FormControlLabelPropsOptions React.Basic.DOM.Props_label) => Prim.Row.Union jss jss_ FormControlLabelClassKeyOptionsJSS => (MUI.Core.Styles.Types.Theme -> Record jss) -> Record given -> React.Basic.JSX
formControlLabelWithStyles style = React.Basic.element (Unsafe.Coerce.unsafeCoerce MUI.Core.Styles.WithStyles.withStyles style _FormControlLabel)
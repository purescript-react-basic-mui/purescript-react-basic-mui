module MUI.Core.StepConnector where

import MUI.Core (JSS) as MUI.Core
import MUI.Core.Styles.Types (Theme) as MUI.Core.Styles.Types
import MUI.Core.Styles.WithStyles (withStyles) as MUI.Core.Styles.WithStyles
import Prelude
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_div) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce
import Unsafe.Reference (unsafeRefEq) as Unsafe.Reference

foreign import data Orientation :: Type

orientation :: { horizontal :: Orientation, vertical :: Orientation }
orientation = { horizontal: Unsafe.Coerce.unsafeCoerce "horizontal", vertical: Unsafe.Coerce.unsafeCoerce "vertical" }

instance eqOrientation :: Eq Orientation where
  eq = Unsafe.Reference.unsafeRefEq

type StepConnectorPropsOptions componentProps = ( active :: Boolean, alternativeLabel :: Boolean, children :: Array React.Basic.JSX, classes :: StepConnectorClassKey, completed :: Boolean, disabled :: Boolean, index :: Number, orientation :: Orientation | componentProps )

foreign import data StepConnectorProps :: Type

foreign import data StepConnectorPropsPartial :: Type

stepConnectorPropsPartial :: ∀ options_ options. Prim.Row.Union options options_ (StepConnectorPropsOptions React.Basic.DOM.Props_div) => Record options -> StepConnectorPropsPartial
stepConnectorPropsPartial = Unsafe.Coerce.unsafeCoerce

type StepConnectorClassKeyGenericOptions a = ( active :: a, alternativeLabel :: a, completed :: a, disabled :: a, horizontal :: a, line :: a, lineHorizontal :: a, lineVertical :: a, root :: a, vertical :: a )

type StepConnectorClassKeyOptions  = StepConnectorClassKeyGenericOptions String

foreign import data StepConnectorClassKey :: Type

stepConnectorClassKey :: ∀ required given. Prim.Row.Union given required StepConnectorClassKeyOptions => Record given -> StepConnectorClassKey
stepConnectorClassKey = Unsafe.Coerce.unsafeCoerce

type StepConnectorClassKeyOptionsJSS  = StepConnectorClassKeyGenericOptions MUI.Core.JSS

foreign import data StepConnectorClassKeyJSS :: Type

stepConnectorClassKeyJSS :: ∀ required given. Prim.Row.Union given required StepConnectorClassKeyOptionsJSS => Record given -> StepConnectorClassKeyJSS
stepConnectorClassKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _StepConnector :: ∀ a. React.Basic.ReactComponent a

stepConnector :: ∀ required given. Prim.Row.Union given required (StepConnectorPropsOptions React.Basic.DOM.Props_div) => Record given -> React.Basic.JSX
stepConnector = React.Basic.element _StepConnector

stepConnector_component :: ∀ required given componentProps. Prim.Row.Union given required (StepConnectorPropsOptions componentProps) => Record given -> React.Basic.JSX
stepConnector_component = React.Basic.element _StepConnector

stepConnectorWithStyles :: ∀ required jss_ jss given. Prim.Row.Union given required (StepConnectorPropsOptions React.Basic.DOM.Props_div) => Prim.Row.Union jss jss_ StepConnectorClassKeyOptionsJSS => (MUI.Core.Styles.Types.Theme -> Record jss) -> Record given -> React.Basic.JSX
stepConnectorWithStyles style = React.Basic.element (Unsafe.Coerce.unsafeCoerce MUI.Core.Styles.WithStyles.withStyles style _StepConnector)
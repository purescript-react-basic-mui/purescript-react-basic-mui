module MUI.Core.Stepper where

import MUI.Core (JSS) as MUI.Core
import MUI.Core.Paper (PaperPropsOptions) as MUI.Core.Paper
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

type StepperPropsOptions componentProps = ( activeStep :: Number, alternativeLabel :: Boolean, children :: Array React.Basic.JSX, classes :: StepperClassKey, connector :: React.Basic.JSX, nonLinear :: Boolean, orientation :: Orientation | componentProps )

foreign import data StepperProps :: Type

foreign import data StepperPropsPartial :: Type

stepperPropsPartial :: ∀ options_ options. Prim.Row.Union options options_ (StepperPropsOptions (MUI.Core.Paper.PaperPropsOptions React.Basic.DOM.Props_div)) => Record options -> StepperPropsPartial
stepperPropsPartial = Unsafe.Coerce.unsafeCoerce

type StepperClassKeyGenericOptions a = ( alternativeLabel :: a, horizontal :: a, root :: a, vertical :: a )

type StepperClassKeyOptions  = StepperClassKeyGenericOptions String

foreign import data StepperClassKey :: Type

stepperClassKey :: ∀ required given. Prim.Row.Union given required StepperClassKeyOptions => Record given -> StepperClassKey
stepperClassKey = Unsafe.Coerce.unsafeCoerce

type StepperClassKeyOptionsJSS  = StepperClassKeyGenericOptions MUI.Core.JSS

foreign import data StepperClassKeyJSS :: Type

stepperClassKeyJSS :: ∀ required given. Prim.Row.Union given required StepperClassKeyOptionsJSS => Record given -> StepperClassKeyJSS
stepperClassKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _Stepper :: ∀ a. React.Basic.ReactComponent a

stepper :: ∀ required given. Prim.Row.Union given required (StepperPropsOptions (MUI.Core.Paper.PaperPropsOptions React.Basic.DOM.Props_div)) => Record given -> React.Basic.JSX
stepper = React.Basic.element _Stepper

stepper_component :: ∀ required given componentProps. Prim.Row.Union given required (StepperPropsOptions componentProps) => Record given -> React.Basic.JSX
stepper_component = React.Basic.element _Stepper

stepperWithStyles :: ∀ required jss_ jss given. Prim.Row.Union given required (StepperPropsOptions (MUI.Core.Paper.PaperPropsOptions React.Basic.DOM.Props_div)) => Prim.Row.Union jss jss_ StepperClassKeyOptionsJSS => (MUI.Core.Styles.Types.Theme -> Record jss) -> Record given -> React.Basic.JSX
stepperWithStyles style = React.Basic.element (Unsafe.Coerce.unsafeCoerce MUI.Core.Styles.WithStyles.withStyles style _Stepper)
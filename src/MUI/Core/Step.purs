module MUI.Core.Step where

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

type StepPropsOptions componentProps = ( active :: Boolean, alternativeLabel :: Boolean, children :: Array React.Basic.JSX, completed :: Boolean, connector :: React.Basic.JSX, disabled :: Boolean, index :: Number, last :: Boolean, orientation :: Orientation | componentProps )

foreign import data StepProps :: Type

foreign import data StepPropsPartial :: Type

stepPropsPartial :: ∀ options_ options. Prim.Row.Union options options_ (StepPropsOptions React.Basic.DOM.Props_div) => Record options -> StepPropsPartial
stepPropsPartial = Unsafe.Coerce.unsafeCoerce

foreign import _Step :: ∀ a. React.Basic.ReactComponent a

step :: ∀ required given. Prim.Row.Union given required (StepPropsOptions React.Basic.DOM.Props_div) => Record given -> React.Basic.JSX
step = React.Basic.element _Step

step_component :: ∀ required given componentProps. Prim.Row.Union given required (StepPropsOptions componentProps) => Record given -> React.Basic.JSX
step_component = React.Basic.element _Step
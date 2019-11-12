module MUI.Core.RadioGroup where

import Foreign (Foreign) as Foreign
import MUI.Core.FormGroup (FormGroupPropsOptions) as MUI.Core.FormGroup
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_div) as React.Basic.DOM
import React.Basic.Events (EventHandler) as React.Basic.Events
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce

type RadioGroupPropsOptions componentProps = ( children :: Array React.Basic.JSX, defaultValue :: Foreign.Foreign, name :: String, onChange :: React.Basic.Events.EventHandler, value :: Foreign.Foreign | componentProps )

foreign import data RadioGroupProps :: Type

foreign import data RadioGroupPropsPartial :: Type

radioGroupPropsPartial :: ∀ options_ options. Prim.Row.Union options options_ (RadioGroupPropsOptions (MUI.Core.FormGroup.FormGroupPropsOptions React.Basic.DOM.Props_div)) => Record options -> RadioGroupPropsPartial
radioGroupPropsPartial = Unsafe.Coerce.unsafeCoerce

foreign import _RadioGroup :: ∀ a. React.Basic.ReactComponent a

radioGroup :: ∀ required given. Prim.Row.Union given required (RadioGroupPropsOptions (MUI.Core.FormGroup.FormGroupPropsOptions React.Basic.DOM.Props_div)) => Record given -> React.Basic.JSX
radioGroup = React.Basic.element _RadioGroup

radioGroup_component :: ∀ required given componentProps. Prim.Row.Union given required (RadioGroupPropsOptions componentProps) => Record given -> React.Basic.JSX
radioGroup_component = React.Basic.element _RadioGroup
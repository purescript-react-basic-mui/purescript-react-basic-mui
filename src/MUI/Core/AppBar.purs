module MUI.Core.AppBar where

import MUI.Core (JSS) as MUI.Core
import MUI.Core.Paper (PaperProps) as MUI.Core.Paper
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, Props_div, ReactComponent) as React.Basic
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce

foreign import data Position :: Type

position :: { absolute ∷ Position, fixed ∷ Position, relative ∷ Position, static ∷ Position, sticky ∷ Position }
position = { absolute: Unsafe.Coerce.unsafeCoerce "absolute", fixed: Unsafe.Coerce.unsafeCoerce "fixed", relative: Unsafe.Coerce.unsafeCoerce "relative", static: Unsafe.Coerce.unsafeCoerce "static", sticky: Unsafe.Coerce.unsafeCoerce "sticky" }

foreign import data Color :: Type

color :: { default ∷ Color, inherit ∷ Color, primary ∷ Color, secondary ∷ Color }
color = { default: Unsafe.Coerce.unsafeCoerce "default", inherit: Unsafe.Coerce.unsafeCoerce "inherit", primary: Unsafe.Coerce.unsafeCoerce "primary", secondary: Unsafe.Coerce.unsafeCoerce "secondary" }

type PropsOptions componentProps = { children ∷ Array (React.Basic.JSX), classes ∷ ClassKey, color ∷ Color, position ∷ Position | componentProps }

type ClassKeyGenericOptions a = ( colorDefault ∷ a, colorPrimary ∷ a, colorSecondary ∷ a, positionAbsolute ∷ a, positionFixed ∷ a, positionRelative ∷ a, positionStatic ∷ a, positionSticky ∷ a, root ∷ a )

type ClassKeyOptions  = ClassKeyGenericOptions String

foreign import data ClassKey :: Type

classKey :: ∀ required given. Prim.Row.Union given required ClassKeyOptions ⇒ Record given → ClassKey
classKey = Unsafe.Coerce.unsafeCoerce

type ClassKeyOptionsJSS  = ClassKeyGenericOptions MUI.Core.JSS

foreign import data ClassKeyJSS :: Type

classKeyJSS :: ∀ required given. Prim.Row.Union given required ClassKeyOptionsJSS ⇒ Record given → ClassKeyJSS
classKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _Component :: ∀ a. React.Basic.ReactComponent a

component :: ∀ required given. Prim.Row.Union given required (PropsOptions (MUI.Core.Paper.PaperProps React.Basic.Props_div)) ⇒ Record given → ClassKeyJSS
component = React.Basic.element _Component

component :: ∀ required given componentProps. Prim.Row.Union given required (PropsOptions componentProps) ⇒ Record given → ClassKeyJSS
component = React.Basic.element _Component

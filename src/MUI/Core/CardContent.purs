module MUI.Core.CardContent where

import MUI.Core (JSS) as MUI.Core
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_div) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce

type CardContentPropsOptions componentProps = ( children :: Array React.Basic.JSX, classes :: CardContentClassKey, component :: React.Basic.ReactComponent {  | componentProps } | componentProps )

foreign import data CardContentProps :: Type

foreign import data CardContentPropsPartial :: Type

cardContentPropsPartial :: ∀ options_ options. Prim.Row.Union options options_ (CardContentPropsOptions React.Basic.DOM.Props_div) => Record options -> CardContentPropsPartial
cardContentPropsPartial = Unsafe.Coerce.unsafeCoerce

type CardContentClassKeyGenericOptions a = ( root :: a )

type CardContentClassKeyOptions  = CardContentClassKeyGenericOptions String

foreign import data CardContentClassKey :: Type

cardContentClassKey :: ∀ required given. Prim.Row.Union given required CardContentClassKeyOptions => Record given -> CardContentClassKey
cardContentClassKey = Unsafe.Coerce.unsafeCoerce

type CardContentClassKeyOptionsJSS  = CardContentClassKeyGenericOptions MUI.Core.JSS

foreign import data CardContentClassKeyJSS :: Type

cardContentClassKeyJSS :: ∀ required given. Prim.Row.Union given required CardContentClassKeyOptionsJSS => Record given -> CardContentClassKeyJSS
cardContentClassKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _CardContent :: ∀ a. React.Basic.ReactComponent a

cardContent :: ∀ required given. Prim.Row.Union given required (CardContentPropsOptions React.Basic.DOM.Props_div) => Record given -> React.Basic.JSX
cardContent = React.Basic.element _CardContent

cardContent_component :: ∀ required given componentProps. Prim.Row.Union given required (CardContentPropsOptions componentProps) => Record given -> React.Basic.JSX
cardContent_component = React.Basic.element _CardContent
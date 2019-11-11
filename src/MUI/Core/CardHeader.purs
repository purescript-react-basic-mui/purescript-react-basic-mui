module MUI.Core.CardHeader where

import MUI.Core (JSS) as MUI.Core
import MUI.Core.Typography (TypographyProps) as MUI.Core.Typography
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_div) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce

type CardHeaderPropsOptions componentProps = ( action :: React.Basic.JSX, avatar :: React.Basic.JSX, children :: Array React.Basic.JSX, classes :: CardHeaderClassKey, component :: React.Basic.ReactComponent {  | componentProps }, disableTypography :: Boolean, subheader :: React.Basic.JSX, subheaderTypographyProps :: MUI.Core.Typography.TypographyProps, title :: React.Basic.JSX, titleTypographyProps :: MUI.Core.Typography.TypographyProps | componentProps )

foreign import data CardHeaderProps :: Type

type CardHeaderClassKeyGenericOptions a = ( action :: a, avatar :: a, content :: a, root :: a, subheader :: a, title :: a )

type CardHeaderClassKeyOptions  = CardHeaderClassKeyGenericOptions String

foreign import data CardHeaderClassKey :: Type

cardHeaderClassKey :: ∀ required given. Prim.Row.Union given required CardHeaderClassKeyOptions => Record given -> CardHeaderClassKey
cardHeaderClassKey = Unsafe.Coerce.unsafeCoerce

type CardHeaderClassKeyOptionsJSS  = CardHeaderClassKeyGenericOptions MUI.Core.JSS

foreign import data CardHeaderClassKeyJSS :: Type

cardHeaderClassKeyJSS :: ∀ required given. Prim.Row.Union given required CardHeaderClassKeyOptionsJSS => Record given -> CardHeaderClassKeyJSS
cardHeaderClassKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _CardHeader :: ∀ a. React.Basic.ReactComponent a

cardHeader :: ∀ required given. Prim.Row.Union given required (CardHeaderPropsOptions React.Basic.DOM.Props_div) => Record given -> React.Basic.JSX
cardHeader = React.Basic.element _CardHeader

cardHeader_component :: ∀ required given componentProps. Prim.Row.Union given required (CardHeaderPropsOptions componentProps) => Record given -> React.Basic.JSX
cardHeader_component = React.Basic.element _CardHeader
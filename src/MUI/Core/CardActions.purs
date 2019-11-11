module MUI.Core.CardActions where

import MUI.Core (JSS) as MUI.Core
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_div) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce

type CardActionsPropsOptions componentProps = ( children :: Array React.Basic.JSX, classes :: CardActionsClassKey, disableSpacing :: Boolean | componentProps )

foreign import data CardActionsProps :: Type

type CardActionsClassKeyGenericOptions a = ( root :: a, spacing :: a )

type CardActionsClassKeyOptions  = CardActionsClassKeyGenericOptions String

foreign import data CardActionsClassKey :: Type

cardActionsClassKey :: ∀ required given. Prim.Row.Union given required CardActionsClassKeyOptions => Record given -> CardActionsClassKey
cardActionsClassKey = Unsafe.Coerce.unsafeCoerce

type CardActionsClassKeyOptionsJSS  = CardActionsClassKeyGenericOptions MUI.Core.JSS

foreign import data CardActionsClassKeyJSS :: Type

cardActionsClassKeyJSS :: ∀ required given. Prim.Row.Union given required CardActionsClassKeyOptionsJSS => Record given -> CardActionsClassKeyJSS
cardActionsClassKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _CardActions :: ∀ a. React.Basic.ReactComponent a

cardActions :: ∀ required given. Prim.Row.Union given required (CardActionsPropsOptions React.Basic.DOM.Props_div) => Record given -> React.Basic.JSX
cardActions = React.Basic.element _CardActions

cardActions_component :: ∀ required given componentProps. Prim.Row.Union given required (CardActionsPropsOptions componentProps) => Record given -> React.Basic.JSX
cardActions_component = React.Basic.element _CardActions
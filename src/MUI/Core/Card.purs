module MUI.Core.Card where

import MUI.Core (JSS) as MUI.Core
import MUI.Core.Paper (PaperProps) as MUI.Core.Paper
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_div) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce

type CardPropsOptions componentProps = ( children :: Array React.Basic.JSX, classes :: CardClassKey, raised :: Boolean | componentProps )

foreign import data CardProps :: Type

foreign import data CardPropsPartial :: Type

type CardClassKeyGenericOptions a = ( root :: a )

type CardClassKeyOptions  = CardClassKeyGenericOptions String

foreign import data CardClassKey :: Type

cardClassKey :: ∀ required given. Prim.Row.Union given required CardClassKeyOptions => Record given -> CardClassKey
cardClassKey = Unsafe.Coerce.unsafeCoerce

type CardClassKeyOptionsJSS  = CardClassKeyGenericOptions MUI.Core.JSS

foreign import data CardClassKeyJSS :: Type

cardClassKeyJSS :: ∀ required given. Prim.Row.Union given required CardClassKeyOptionsJSS => Record given -> CardClassKeyJSS
cardClassKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _Card :: ∀ a. React.Basic.ReactComponent a

card :: ∀ required given. Prim.Row.Union given required (CardPropsOptions (MUI.Core.Paper.PaperProps React.Basic.DOM.Props_div)) => Record given -> React.Basic.JSX
card = React.Basic.element _Card

card_component :: ∀ required given componentProps. Prim.Row.Union given required (CardPropsOptions componentProps) => Record given -> React.Basic.JSX
card_component = React.Basic.element _Card
module MUI.Core.CardMedia where

import MUI.Core (JSS) as MUI.Core
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_div) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce

type CardMediaPropsOptions componentProps = ( children :: Array React.Basic.JSX, classes :: CardMediaClassKey, image :: String, src :: String | componentProps )

foreign import data CardMediaProps :: Type

foreign import data CardMediaPropsPartial :: Type

cardMediaPropsPartial :: ∀ options_ options. Prim.Row.Union options options_ (CardMediaPropsOptions React.Basic.DOM.Props_div) => Record options -> CardMediaPropsPartial
cardMediaPropsPartial = Unsafe.Coerce.unsafeCoerce

type CardMediaClassKeyGenericOptions a = ( media :: a, root :: a )

type CardMediaClassKeyOptions  = CardMediaClassKeyGenericOptions String

foreign import data CardMediaClassKey :: Type

cardMediaClassKey :: ∀ required given. Prim.Row.Union given required CardMediaClassKeyOptions => Record given -> CardMediaClassKey
cardMediaClassKey = Unsafe.Coerce.unsafeCoerce

type CardMediaClassKeyOptionsJSS  = CardMediaClassKeyGenericOptions MUI.Core.JSS

foreign import data CardMediaClassKeyJSS :: Type

cardMediaClassKeyJSS :: ∀ required given. Prim.Row.Union given required CardMediaClassKeyOptionsJSS => Record given -> CardMediaClassKeyJSS
cardMediaClassKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _CardMedia :: ∀ a. React.Basic.ReactComponent a

cardMedia :: ∀ required given. Prim.Row.Union given required (CardMediaPropsOptions React.Basic.DOM.Props_div) => Record given -> React.Basic.JSX
cardMedia = React.Basic.element _CardMedia

cardMedia_component :: ∀ required given componentProps. Prim.Row.Union given required (CardMediaPropsOptions componentProps) => Record given -> React.Basic.JSX
cardMedia_component = React.Basic.element _CardMedia
module MUI.Core.Paper where

import MUI.Core (JSS) as MUI.Core
import MUI.Core.Styles.Types (Theme) as MUI.Core.Styles.Types
import MUI.Core.Styles.WithStyles (withStyles) as MUI.Core.Styles.WithStyles
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_div) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce

type PaperPropsOptions componentProps = ( children :: Array React.Basic.JSX, classes :: PaperClassKey, component :: React.Basic.ReactComponent {  | componentProps }, elevation :: Number, square :: Boolean | componentProps )

foreign import data PaperProps :: Type

foreign import data PaperPropsPartial :: Type

paperPropsPartial :: ∀ options_ options. Prim.Row.Union options options_ (PaperPropsOptions React.Basic.DOM.Props_div) => Record options -> PaperPropsPartial
paperPropsPartial = Unsafe.Coerce.unsafeCoerce

type PaperClassKeyGenericOptions a = ( elevation0 :: a, elevation1 :: a, elevation10 :: a, elevation11 :: a, elevation12 :: a, elevation13 :: a, elevation14 :: a, elevation15 :: a, elevation16 :: a, elevation17 :: a, elevation18 :: a, elevation19 :: a, elevation2 :: a, elevation20 :: a, elevation21 :: a, elevation22 :: a, elevation23 :: a, elevation24 :: a, elevation3 :: a, elevation4 :: a, elevation5 :: a, elevation6 :: a, elevation7 :: a, elevation8 :: a, elevation9 :: a, outlined :: a, root :: a, rounded :: a )

type PaperClassKeyOptions  = PaperClassKeyGenericOptions String

foreign import data PaperClassKey :: Type

paperClassKey :: ∀ required given. Prim.Row.Union given required PaperClassKeyOptions => Record given -> PaperClassKey
paperClassKey = Unsafe.Coerce.unsafeCoerce

type PaperClassKeyOptionsJSS  = PaperClassKeyGenericOptions MUI.Core.JSS

foreign import data PaperClassKeyJSS :: Type

paperClassKeyJSS :: ∀ required given. Prim.Row.Union given required PaperClassKeyOptionsJSS => Record given -> PaperClassKeyJSS
paperClassKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _Paper :: ∀ a. React.Basic.ReactComponent a

paper :: ∀ required given. Prim.Row.Union given required (PaperPropsOptions React.Basic.DOM.Props_div) => Record given -> React.Basic.JSX
paper = React.Basic.element _Paper

paper_component :: ∀ required given componentProps. Prim.Row.Union given required (PaperPropsOptions componentProps) => Record given -> React.Basic.JSX
paper_component = React.Basic.element _Paper

paperWithStyles :: ∀ required jss_ jss given. Prim.Row.Union given required (PaperPropsOptions React.Basic.DOM.Props_div) => Prim.Row.Union jss jss_ PaperClassKeyOptionsJSS => (MUI.Core.Styles.Types.Theme -> Record jss) -> Record given -> React.Basic.JSX
paperWithStyles style = React.Basic.element (Unsafe.Coerce.unsafeCoerce MUI.Core.Styles.WithStyles.withStyles style _Paper)
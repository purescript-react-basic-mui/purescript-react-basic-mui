module MUI.Core.Hidden where

import Foreign (Foreign) as Foreign
import Prelude
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_div) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce
import Unsafe.Reference (unsafeRefEq) as Unsafe.Reference

foreign import data InitialWidth :: Type

initialWidth :: { lg :: InitialWidth, md :: InitialWidth, sm :: InitialWidth, xl :: InitialWidth, xs :: InitialWidth }
initialWidth = { lg: Unsafe.Coerce.unsafeCoerce "lg", md: Unsafe.Coerce.unsafeCoerce "md", sm: Unsafe.Coerce.unsafeCoerce "sm", xl: Unsafe.Coerce.unsafeCoerce "xl", xs: Unsafe.Coerce.unsafeCoerce "xs" }

foreign import data Implementation :: Type

implementation :: { css :: Implementation, js :: Implementation }
implementation = { css: Unsafe.Coerce.unsafeCoerce "css", js: Unsafe.Coerce.unsafeCoerce "js" }

instance eqImplementation :: Eq Implementation where
  eq = Unsafe.Reference.unsafeRefEq

instance eqInitialWidth :: Eq InitialWidth where
  eq = Unsafe.Reference.unsafeRefEq

type HiddenPropsOptions componentProps = ( implementation :: Implementation, initialWidth :: InitialWidth, lgDown :: Boolean, lgUp :: Boolean, mdDown :: Boolean, mdUp :: Boolean, only :: Foreign.Foreign, smDown :: Boolean, smUp :: Boolean, xlDown :: Boolean, xlUp :: Boolean, xsDown :: Boolean, xsUp :: Boolean | componentProps )

foreign import data HiddenProps :: Type

foreign import data HiddenPropsPartial :: Type

hiddenPropsPartial :: ∀ options_ options. Prim.Row.Union options options_ (HiddenPropsOptions React.Basic.DOM.Props_div) => Record options -> HiddenPropsPartial
hiddenPropsPartial = Unsafe.Coerce.unsafeCoerce

foreign import _Hidden :: ∀ a. React.Basic.ReactComponent a

hidden :: ∀ required given. Prim.Row.Union given required (HiddenPropsOptions React.Basic.DOM.Props_div) => Record given -> React.Basic.JSX
hidden = React.Basic.element _Hidden

hidden_component :: ∀ required given componentProps. Prim.Row.Union given required (HiddenPropsOptions componentProps) => Record given -> React.Basic.JSX
hidden_component = React.Basic.element _Hidden
module MUI.Core.Container where

import Prelude
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_div) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce
import Unsafe.Reference (unsafeRefEq) as Unsafe.Reference

foreign import data MaxWidth :: Type

maxWidth :: { "false" :: MaxWidth, lg :: MaxWidth, md :: MaxWidth, sm :: MaxWidth, xl :: MaxWidth, xs :: MaxWidth }
maxWidth = { "false": Unsafe.Coerce.unsafeCoerce false, lg: Unsafe.Coerce.unsafeCoerce "lg", md: Unsafe.Coerce.unsafeCoerce "md", sm: Unsafe.Coerce.unsafeCoerce "sm", xl: Unsafe.Coerce.unsafeCoerce "xl", xs: Unsafe.Coerce.unsafeCoerce "xs" }

instance eqMaxWidth :: Eq MaxWidth where
  eq = Unsafe.Reference.unsafeRefEq

type ContainerPropsOptions componentProps = ( component :: React.Basic.ReactComponent {  | componentProps }, fixed :: Boolean, maxWidth :: MaxWidth | componentProps )

foreign import data ContainerProps :: Type

foreign import data ContainerPropsPartial :: Type

containerPropsPartial :: ∀ options_ options. Prim.Row.Union options options_ (ContainerPropsOptions React.Basic.DOM.Props_div) => Record options -> ContainerPropsPartial
containerPropsPartial = Unsafe.Coerce.unsafeCoerce

foreign import _Container :: ∀ a. React.Basic.ReactComponent a

container :: ∀ required given. Prim.Row.Union given required (ContainerPropsOptions React.Basic.DOM.Props_div) => Record given -> React.Basic.JSX
container = React.Basic.element _Container

container_component :: ∀ required given componentProps. Prim.Row.Union given required (ContainerPropsOptions componentProps) => Record given -> React.Basic.JSX
container_component = React.Basic.element _Container
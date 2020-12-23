module MUI.React.Basic where

import React.Basic (JSX, ReactComponent)
import React.Basic (element) as React.Basic
import Unsafe.Coerce (unsafeCoerce)

element ::
  forall props.
  ReactComponent props ->
  props ->
  JSX
element = unsafeCoerce React.Basic.element

module React.Basic.MUI.Core.CssBaseline where 

import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic (element, ReactComponent, JSX)

type CssBaselineProps_optional =
  ( children :: Foreign
  )

foreign import data CssBaselineProps :: Type 

cssBaselineProps
  :: ∀ attrs attrs_
   . Union attrs attrs_ (CssBaselineProps_optional )
  => Record (attrs)
  -> CssBaselineProps
cssBaselineProps = unsafeCoerce

cssBaseline
  :: ∀ attrs attrs_
   . Union attrs attrs_ (CssBaselineProps_optional )
  => Record (attrs)
  -> JSX
cssBaseline = element _CssBaseline
foreign import _CssBaseline :: forall a. ReactComponent a 

type CssBaselineClassKey = String
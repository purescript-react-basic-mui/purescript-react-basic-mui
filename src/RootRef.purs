module React.Basic.MUI.RootRef where 

import Prelude
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic (element, ReactComponent, JSX)

type RootRefProps_optional t =
  ( rootRef :: Foreign
  )

foreign import data RootRefProps :: Type 

rootRefProps
  :: ∀ attrs attrs_
   . Union attrs attrs_ (RootRefProps_optional)
  => Record (attrs)
  -> RootRefProps
rootRefProps = unsafeCoerce

rootRef
  :: ∀ attrs attrs_
   . Union attrs attrs_ (RootRefProps_optional)
  => Record (attrs)
  -> JSX
rootRef = element _RootRef
foreign import _RootRef :: forall a. ReactComponent a 
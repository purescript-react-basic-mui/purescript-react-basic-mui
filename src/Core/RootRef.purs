module React.Basic.MUI.Core.RootRef where 

import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic (element, ReactComponent, JSX)

type RootRefProps_optional t =
  ( rootRef :: Foreign
  )

foreign import data RootRefProps :: Type 

rootRefProps
  :: ∀ t attrs attrs_
   . Union attrs attrs_ (RootRefProps_optional t )
  => Record (attrs)
  -> RootRefProps
rootRefProps = unsafeCoerce

rootRef
  :: ∀ t attrs attrs_
   . Union attrs attrs_ (RootRefProps_optional t )
  => Record (attrs)
  -> JSX
rootRef = element _RootRef
foreign import _RootRef :: forall a. ReactComponent a 

module React.Basic.MUI.Core.NoSsr where 

import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic (element, ReactComponent, JSX)

type NoSsrProps_required  optional =
  ( children :: JSX
  | optional )

type NoSsrProps_optional =
  ( defer :: Boolean
  , fallback :: JSX
  )

foreign import data NoSsrProps :: Type 

noSsrProps
  :: ∀ attrs attrs_
   . Union attrs attrs_ (NoSsrProps_optional )
  => Record (NoSsrProps_required attrs)
  -> NoSsrProps
noSsrProps = unsafeCoerce

noSsr
  :: ∀ attrs attrs_
   . Union attrs attrs_ (NoSsrProps_optional )
  => Record (NoSsrProps_required attrs)
  -> JSX
noSsr = element _NoSsr
foreign import _NoSsr :: forall a. ReactComponent a 
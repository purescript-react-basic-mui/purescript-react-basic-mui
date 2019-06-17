module React.Basic.MUI.Core.Portal where 

import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)




type PortalProps_required  optional =
  ( children :: Foreign
  | optional )

type PortalProps_optional =
  ( container :: Foreign
  , disablePortal :: Boolean
  , onRendered :: Foreign
  )

foreign import data PortalProps :: Type 

portalProps
  :: ∀ attrs attrs_
   . Union attrs attrs_ (PortalProps_optional )
  => Record (PortalProps_required attrs)
  -> PortalProps
portalProps = unsafeCoerce
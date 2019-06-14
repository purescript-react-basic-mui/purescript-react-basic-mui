module React.Basic.MUI.Hidden where 

import Prelude
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic.MUI.Styles.CreateBreakpoints (Breakpoint)
import React.Basic (element, ReactComponent, JSX)

type HiddenProps_optional =
  ( implementation :: Foreign
  , initialWidth :: Breakpoint 
  , lgDown :: Boolean
  , lgUp :: Boolean
  , mdDown :: Boolean
  , mdUp :: Boolean
  , only :: Foreign
  , smDown :: Boolean
  , smUp :: Boolean
  , xlDown :: Boolean
  , xlUp :: Boolean
  , xsDown :: Boolean
  , xsUp :: Boolean
  )

foreign import data HiddenProps :: Type 

hiddenProps
  :: ∀ attrs attrs_
   . Union attrs attrs_ (HiddenProps_optional)
  => Record (attrs)
  -> HiddenProps
hiddenProps = unsafeCoerce

hidden
  :: ∀ attrs attrs_
   . Union attrs attrs_ (HiddenProps_optional)
  => Record (attrs)
  -> JSX
hidden = element _Hidden
foreign import _Hidden :: forall a. ReactComponent a 
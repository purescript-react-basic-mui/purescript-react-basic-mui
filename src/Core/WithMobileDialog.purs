module React.Basic.MUI.Core.WithMobileDialog where 

import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic.MUI.Core.Styles.CreateBreakpoints (Breakpoint)

type WithMobileDialogOptions_required  optional =
  ( breakpoint :: Breakpoint 
  | optional )

type WithMobileDialogOptions_optional =
  ( 
  )

foreign import data WithMobileDialogOptions :: Type 



type WithMobileDialog_required  optional =
  ( fullScreen :: Boolean
  , width :: Breakpoint 
  | optional )

type WithMobileDialog_optional =
  ( 
  )

foreign import data WithMobileDialog :: Type 



type InjectedProps_required  optional =
  ( fullScreen :: Boolean
  , width :: Breakpoint 
  | optional )

type InjectedProps_optional =
  ( 
  )

foreign import data InjectedProps :: Type 

injectedProps
  :: ∀ attrs attrs_
   . Union attrs attrs_ (InjectedProps_optional )
  => Record (InjectedProps_required attrs)
  -> InjectedProps
injectedProps = unsafeCoerce

withMobileDialog :: Foreign
withMobileDialog = _withMobileDialog
foreign import _withMobileDialog :: Foreign
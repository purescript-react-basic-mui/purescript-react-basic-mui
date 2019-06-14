module React.Basic.MUI.WithMobileDialog where 

import Prelude
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic.MUI.Styles.CreateBreakpoints (Breakpoint)

type WithMobileDialogOptions_required optional =
  ( breakpoint :: Breakpoint 
  | optional )

type WithMobileDialogOptions_optional =
  ( 
  )

foreign import data WithMobileDialogOptions :: Type 

withMobileDialogOptions
  :: ∀ attrs attrs_
   . Union attrs attrs_ (WithMobileDialogOptions_optional)
  => Record (WithMobileDialogOptions_required attrs)
  -> WithMobileDialogOptions
withMobileDialogOptions = unsafeCoerce

type WithMobileDialog_required optional =
  ( fullScreen :: Boolean
  , width :: Breakpoint 
  | optional )

type WithMobileDialog_optional =
  ( 
  )

foreign import data WithMobileDialog :: Type 

withMobileDialog
  :: ∀ attrs attrs_
   . Union attrs attrs_ (WithMobileDialog_optional)
  => Record (WithMobileDialog_required attrs)
  -> WithMobileDialog
withMobileDialog = unsafeCoerce

type InjectedProps_required optional =
  ( fullScreen :: Boolean
  , width :: Breakpoint 
  | optional )

type InjectedProps_optional =
  ( 
  )

foreign import data InjectedProps :: Type 

injectedProps
  :: ∀ attrs attrs_
   . Union attrs attrs_ (InjectedProps_optional)
  => Record (InjectedProps_required attrs)
  -> InjectedProps
injectedProps = unsafeCoerce

withMobileDialog :: PropInjector WithMobileDialog  Foreign
withMobileDialog = _withMobileDialog
foreign import _withMobileDialog :: PropInjector WithMobileDialog  Foreign
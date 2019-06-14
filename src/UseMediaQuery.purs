module React.Basic.MUI.UseMediaQuery where 

import Prelude
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)




type MuiMediaQueryListEvent_required optional =
  ( matches :: Boolean
  | optional )

type MuiMediaQueryListEvent_optional =
  ( 
  )

foreign import data MuiMediaQueryListEvent :: Type 

muiMediaQueryListEvent
  :: ∀ attrs attrs_
   . Union attrs attrs_ (MuiMediaQueryListEvent_optional)
  => Record (MuiMediaQueryListEvent_required attrs)
  -> MuiMediaQueryListEvent
muiMediaQueryListEvent = unsafeCoerce

type MuiMediaQueryList_required optional =
  ( matches :: Boolean
  , addListener :: Foreign
  , removeListener :: Foreign
  | optional )

type MuiMediaQueryList_optional =
  ( 
  )

foreign import data MuiMediaQueryList :: Type 

muiMediaQueryList
  :: ∀ attrs attrs_
   . Union attrs attrs_ (MuiMediaQueryList_optional)
  => Record (MuiMediaQueryList_required attrs)
  -> MuiMediaQueryList
muiMediaQueryList = unsafeCoerce

type MuiMediaQueryListListener = Foreign

type Options_optional =
  ( defaultMatches :: Boolean
  , noSsr :: Boolean
  , ssrMatchMedia :: Foreign
  )

foreign import data Options :: Type 

options
  :: ∀ attrs attrs_
   . Union attrs attrs_ (Options_optional)
  => Record (attrs)
  -> Options
options = unsafeCoerce

useMediaQuery :: Boolean
useMediaQuery = _useMediaQuery
foreign import _useMediaQuery :: Boolean
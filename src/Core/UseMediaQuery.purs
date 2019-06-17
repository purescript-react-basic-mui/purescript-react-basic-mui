module React.Basic.MUI.Core.UseMediaQuery where 

import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)




type MuiMediaQueryListEvent_required  optional =
  ( matches :: Boolean
  | optional )

type MuiMediaQueryListEvent_optional =
  ( 
  )

foreign import data MuiMediaQueryListEvent :: Type 



type MuiMediaQueryList_required  optional =
  ( matches :: Boolean
  , addListener :: Foreign
  , removeListener :: Foreign
  | optional )

type MuiMediaQueryList_optional =
  ( 
  )

foreign import data MuiMediaQueryList :: Type 



type MuiMediaQueryListListener = Foreign

type Options_optional =
  ( defaultMatches :: Boolean
  , noSsr :: Boolean
  , ssrMatchMedia :: Foreign
  )

foreign import data Options :: Type 



useMediaQuery :: Boolean
useMediaQuery = _useMediaQuery
foreign import _useMediaQuery :: Boolean
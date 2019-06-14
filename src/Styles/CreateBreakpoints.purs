module React.Basic.MUI.Styles.CreateBreakpoints where 

import Prelude
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)




type Breakpoint = Foreign

type BreakpointValues = Foreign

keys :: (Array Breakpoint )
keys = _keys
foreign import _keys :: (Array Breakpoint )

type Breakpoints_required optional =
  ( keys :: (Array Breakpoint )
  , values :: BreakpointValues 
  , up :: Foreign
  , down :: Foreign
  , between :: Foreign
  , only :: Foreign
  , width :: Foreign
  | optional )

type Breakpoints_optional =
  ( 
  )

foreign import data Breakpoints :: Type 

breakpoints
  :: ∀ attrs attrs_
   . Union attrs attrs_ (Breakpoints_optional)
  => Record (Breakpoints_required attrs)
  -> Breakpoints
breakpoints = unsafeCoerce

type BreakpointsOptions = Foreign

createBreakpoints :: Breakpoints 
createBreakpoints = _createBreakpoints
foreign import _createBreakpoints :: Breakpoints 
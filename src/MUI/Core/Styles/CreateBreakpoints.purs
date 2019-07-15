module MUI.Core.Styles.CreateBreakpoints where

import Prelude

import Data.Function.Uncurried (Fn2)
import Foreign (Foreign, unsafeToForeign)
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)

type BreakpointValues =
  { xs :: Number
  , sm :: Number
  , md :: Number
  , lg :: Number
  , xl :: Number
  }

type Breakpoints =
  { keys :: Array String
  , values :: BreakpointValues
  , up :: String -> String
  , down :: String -> String
  , between :: Fn2 String String String 
  , only :: String -> String
  , width :: String -> Number
  }

type BreakpointsPartial =
  ( unit :: String
  , step :: Number
  , keys :: Array String
  , values :: BreakpointValues
  , up :: String -> String
  , down :: String -> String
  , between :: Fn2 String String String 
  , only :: String -> String
  , width :: String -> Number
  )

foreign import data BreakpointsOptions :: Type

breakpointsOptions :: ∀ options options_
  . Union options options_ BreakpointsPartial
  => Record options
  -> BreakpointsOptions
breakpointsOptions = unsafeCoerce

createBreakPoints :: ∀ options options_
  . Union options options_ BreakpointsPartial
  => Record options 
  -> Breakpoints 
createBreakPoints = _createBreakpoints <<< unsafeToForeign


foreign import _createBreakpoints :: Foreign -> Breakpoints 
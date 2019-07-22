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
  { keys :: Array BreakpointProp
  , values :: BreakpointValues
  , up :: BreakpointProp -> String
  , down :: BreakpointProp -> String
  , between :: Fn2 BreakpointProp BreakpointProp String 
  , only :: BreakpointProp -> String
  , width :: BreakpointProp -> Number
  }

type BreakpointsPartial =
  ( unit :: String
  , step :: Number
  , keys :: Array BreakpointProp
  , values :: BreakpointValues
  , up :: BreakpointProp -> String
  , down :: BreakpointProp -> String
  , between :: Fn2 BreakpointProp BreakpointProp String 
  , only :: BreakpointProp -> String
  , width :: BreakpointProp -> Number
  )

foreign import data BreakpointProp :: Type
data Breakpoint = XS | SM | MD | LG | XL
breakpoint :: Breakpoint -> BreakpointProp
breakpoint XS = unsafeCoerce "xs"
breakpoint SM = unsafeCoerce "sm"
breakpoint MD = unsafeCoerce "md"
breakpoint LG = unsafeCoerce "lg"
breakpoint XL = unsafeCoerce "xl"

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
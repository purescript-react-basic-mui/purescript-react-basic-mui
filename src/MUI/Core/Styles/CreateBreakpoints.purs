module MUI.Core.Styles.CreateBreakpoints where

import Prelude
import Data.Function.Uncurried (Fn2)
import Foreign (Foreign, unsafeToForeign)
import MUI.Core (MediaQuery)
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)

type BreakpointValues
  = { xs :: Number
    , sm :: Number
    , md :: Number
    , lg :: Number
    , xl :: Number
    }

type Breakpoints
  = { keys :: Array Breakpoint
    , values :: BreakpointValues
    , up :: Breakpoint -> MediaQuery
    , down :: Breakpoint -> MediaQuery
    , between :: Fn2 Breakpoint Breakpoint MediaQuery
    , only :: Breakpoint -> MediaQuery
    , width :: Breakpoint -> Number
    }

foreign import data Breakpoint :: Type

xs :: Breakpoint
xs = unsafeCoerce "xs"

sm :: Breakpoint
sm = unsafeCoerce "sm"

md :: Breakpoint
md = unsafeCoerce "md"

lg :: Breakpoint
lg = unsafeCoerce "lg"

xl :: Breakpoint
xl = unsafeCoerce "xl"

createBreakPoints ::
  forall options options_.
  Union options options_ ( values :: BreakpointValues ) =>
  Record options ->
  Breakpoints
createBreakPoints = unsafeCreateBreakpoints <<< unsafeToForeign

foreign import unsafeCreateBreakpoints :: Foreign -> Breakpoints

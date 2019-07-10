module MUI.Core.Styles.CreateBreakpoints where

import Prelude

import Data.Function.Uncurried (Fn2)
import Data.Maybe (Maybe(..))
import Foreign (Foreign)
import MUI.Core (FnStringStringToString, StringToNumber, StringToString)
import Simple.JSON (write)

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

type BreakpointsOptions =
  { unit :: Maybe String
  , step :: Maybe Number
  , keys :: Maybe (Array String)
  , values :: Maybe BreakpointValues
  , up :: Maybe StringToString 
  , down :: Maybe StringToString 
  , between :: Maybe FnStringStringToString 
  , only :: Maybe StringToString 
  , width :: Maybe StringToNumber
  }

breakpointsOptions :: BreakpointsOptions
breakpointsOptions =
  { unit : Nothing
  , step : Nothing
  , keys : Nothing
  , values : Nothing
  , up : Nothing
  , down : Nothing
  , between : Nothing
  , only : Nothing
  , width : Nothing
  }

createBreakpoints :: BreakpointsOptions -> Breakpoints
createBreakpoints = write >>> _createBreakpoints

foreign import _createBreakpoints :: Foreign -> Breakpoints 
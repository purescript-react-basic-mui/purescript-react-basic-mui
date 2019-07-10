module MUI.Core.Styles.Transitions where

import Data.Maybe (Maybe(..))
import Foreign (Foreign)
import MUI.Core (NumberToNumber)

type EasingOptions =
  { easeInOut :: Maybe String
  , easeOut :: Maybe String
  , easeIn :: Maybe String
  , sharp :: Maybe String
  }

type Easing =
  { easeInOut :: String
  , easeOut :: String
  , easeIn :: String
  , sharp :: String
  }

foreign import easing :: Easing

type Duration =
  { shortest :: Number
  , shorter :: Number
  , short :: Number
  , standard :: Number
  , complex :: Number
  , enteringScreen :: Number
  , leavingScreen :: Number
  }

type Transitions =
  { easing :: Easing
  , duration :: Duration
  , create :: Foreign
  , getAutoHeightDuration :: Number -> Number
  }

type TransitionsOptions =
  { easing :: Maybe Easing
  , duration :: Maybe Duration
  , create :: Maybe Foreign
  , getAutoHeightDuration :: Maybe NumberToNumber
  } 

transitionsOptions :: TransitionsOptions
transitionsOptions = 
  { easing : Nothing
  , duration : Nothing
  , create : Nothing
  , getAutoHeightDuration : Nothing
  }

foreign import duration :: Duration 
foreign import formatMs :: Number -> String
foreign import transitions :: Transitions

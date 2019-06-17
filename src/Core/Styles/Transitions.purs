module React.Basic.MUI.Core.Styles.Transitions where 

import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)




type Easing_required  optional =
  ( easeInOut :: String
  , easeOut :: String
  , easeIn :: String
  , sharp :: String
  | optional )

type Easing_optional =
  ( 
  )

foreign import data Easing :: Type 



easing :: Easing 
easing = _easing
foreign import _easing :: Easing 

type Duration_required  optional =
  ( shortest :: Number
  , shorter :: Number
  , short :: Number
  , standard :: Number
  , complex :: Number
  , enteringScreen :: Number
  , leavingScreen :: Number
  | optional )

type Duration_optional =
  ( 
  )

foreign import data Duration :: Type 



duration :: Duration 
duration = _duration
foreign import _duration :: Duration 

formatMs :: String
formatMs = _formatMs
foreign import _formatMs :: String

type Transitions_required  optional =
  ( easing :: Easing 
  , duration :: Duration 
  , create :: Foreign
  , getAutoHeightDuration :: Foreign
  | optional )

type Transitions_optional =
  ( 
  )

foreign import data Transitions :: Type 



type TransitionsOptions_optional =
  ( easing :: Foreign
  , duration :: Foreign
  , create :: Foreign
  , getAutoHeightDuration :: Foreign
  )

foreign import data TransitionsOptions :: Type 



transitions :: Transitions 
transitions = _transitions
foreign import _transitions :: Transitions 
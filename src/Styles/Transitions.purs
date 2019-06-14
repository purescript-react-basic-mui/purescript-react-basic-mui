module React.Basic.MUI.Styles.Transitions where 

import Prelude
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)




type Easing_required optional =
  ( easeInOut :: String
  , easeOut :: String
  , easeIn :: String
  , sharp :: String
  | optional )

type Easing_optional =
  ( 
  )

foreign import data Easing :: Type 

easing
  :: ∀ attrs attrs_
   . Union attrs attrs_ (Easing_optional)
  => Record (Easing_required attrs)
  -> Easing
easing = unsafeCoerce

easing :: Easing 
easing = _easing
foreign import _easing :: Easing 

type Duration_required optional =
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

duration
  :: ∀ attrs attrs_
   . Union attrs attrs_ (Duration_optional)
  => Record (Duration_required attrs)
  -> Duration
duration = unsafeCoerce

duration :: Duration 
duration = _duration
foreign import _duration :: Duration 

formatMs :: String
formatMs = _formatMs
foreign import _formatMs :: String

type Transitions_required optional =
  ( easing :: Easing 
  , duration :: Duration 
  , create :: Foreign
  , getAutoHeightDuration :: Foreign
  | optional )

type Transitions_optional =
  ( 
  )

foreign import data Transitions :: Type 

transitions
  :: ∀ attrs attrs_
   . Union attrs attrs_ (Transitions_optional)
  => Record (Transitions_required attrs)
  -> Transitions
transitions = unsafeCoerce

type TransitionsOptions_optional =
  ( easing :: Foreign
  , duration :: Foreign
  , create :: Foreign
  , getAutoHeightDuration :: Foreign
  )

foreign import data TransitionsOptions :: Type 

transitionsOptions
  :: ∀ attrs attrs_
   . Union attrs attrs_ (TransitionsOptions_optional)
  => Record (attrs)
  -> TransitionsOptions
transitionsOptions = unsafeCoerce

transitions :: Transitions 
transitions = _transitions
foreign import _transitions :: Transitions 
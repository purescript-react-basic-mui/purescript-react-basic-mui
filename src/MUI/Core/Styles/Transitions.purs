module MUI.Core.Styles.Transitions where

import Foreign (Foreign)
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)

type EasingPartial
  = ( easeInOut :: String
    , easeOut :: String
    , easeIn :: String
    , sharp :: String
    )

foreign import data EasingOptions :: Type

type Easing
  = Record EasingPartial

type DurationPartial
  = ( shortest :: Number
    , shorter :: Number
    , short :: Number
    , standard :: Number
    , complex :: Number
    , enteringScreen :: Number
    , leavingScreen :: Number
    )

type Duration
  = Record DurationPartial

type TransitionsPartial
  = ( easing :: Easing
    , duration :: Duration
    , create :: Foreign
    , getAutoHeightDuration :: Number -> Number
    )

foreign import data TransitionsOptions :: Type

type Transitions
  = Record TransitionsPartial

transitionsOptions ::
  ∀ options options_.
  Union options options_ TransitionsPartial =>
  Record options ->
  TransitionsOptions
transitionsOptions = unsafeCoerce

foreign import easing :: Easing

foreign import duration :: Duration

foreign import formatMs :: Number -> String

foreign import transitions :: Transitions

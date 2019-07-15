module MUI.Core.LinearProgress where

import React.Basic (JSX, ReactComponent, element)
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)

type LinearProgressProps =
  ( classes :: LinearProgressClassKey
  , color :: String
  , value :: Number
  , valueBuffer :: Number
  , variant :: String
  )

foreign import data LinearProgressClassKey :: Type
foreign import data LinearProgressPropsPartial :: Type

type LinearProgressClassKeyOptions =
  ( root :: String
  , colorPrimary :: String
  , colorSecondary :: String
  , determinate :: String
  , indeterminate :: String
  , buffer :: String
  , query :: String
  , dashed :: String
  , dashedColorPrimary :: String
  , dashedColorSecondary :: String
  , bar :: String
  , barColorPrimary :: String
  , barColorSecondary :: String
  , bar1Indeterminate :: String
  , bar1Determinate :: String
  , bar1Buffer :: String
  , bar2Indeterminate :: String
  , bar2Buffer :: String
  )

linearProgressClassKey :: ∀ options options_
  . Union options options_ LinearProgressClassKeyOptions
  => Record options
  -> LinearProgressClassKey
linearProgressClassKey = unsafeCoerce

linearProgress :: ∀ props props_
  . Union props props_ LinearProgressProps
  => Record props 
  -> JSX
linearProgress = element _LinearProgress

linearProgressPropsPartial :: ∀ props props_
  . Union props props_ LinearProgressProps
  => Record props 
  -> LinearProgressPropsPartial 
linearProgressPropsPartial = unsafeCoerce

foreign import _LinearProgress :: ∀ a. ReactComponent a
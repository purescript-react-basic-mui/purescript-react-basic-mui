module MUI.Core.LinearProgress where

import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_div)
import Unsafe.Coerce (unsafeCoerce)

type LinearProgressProps componentProps =
  ( classes :: LinearProgressClassKey
  , color :: ColorProp
  , value :: Number
  , valueBuffer :: Number
  , variant :: Variant
  | componentProps
  )

foreign import data ColorProp :: Type
data Color = Primary | Secondary
color :: Color -> ColorProp
color Primary = unsafeCoerce "primary"
color Secondary = unsafeCoerce "secondary"

foreign import data VariantProp :: Type
data Variant = Determinate | Indeterminate | Buffer | Query
variant :: Variant -> VariantProp
variant Determinate = unsafeCoerce "determinate"
variant Indeterminate = unsafeCoerce "indeterminate"
variant Buffer = unsafeCoerce "buffer"
variant Query = unsafeCoerce "query"

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
  . Union props props_ (LinearProgressProps Props_div)
  => Record props 
  -> JSX
linearProgress = element _LinearProgress

linearProgressPropsPartial :: ∀ props props_
  . Union props props_ (LinearProgressProps Props_div)
  => Record props 
  -> LinearProgressPropsPartial 
linearProgressPropsPartial = unsafeCoerce

foreign import _LinearProgress :: ∀ a. ReactComponent a
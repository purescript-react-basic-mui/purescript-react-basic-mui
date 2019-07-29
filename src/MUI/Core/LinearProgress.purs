module MUI.Core.LinearProgress where

import MUI.Core (JSS)
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
foreign import data LinearProgressClassKeyJSS :: Type
foreign import data LinearProgressPropsPartial :: Type

type LinearProgressClassKeyOptionsJSS = LinearProgressClassKeyOptionsR JSS
type LinearProgressClassKeyOptions = LinearProgressClassKeyOptionsR String
type LinearProgressClassKeyOptionsR a =
  ( root :: a
  , colorPrimary :: a
  , colorSecondary :: a
  , determinate :: a
  , indeterminate :: a
  , buffer :: a
  , query :: a
  , dashed :: a
  , dashedColorPrimary :: a
  , dashedColorSecondary :: a
  , bar :: a
  , barColorPrimary :: a
  , barColorSecondary :: a
  , bar1Indeterminate :: a
  , bar1Determinate :: a
  , bar1Buffer :: a
  , bar2Indeterminate :: a
  , bar2Buffer :: a
  )

linearProgressClassKey :: ∀ options options_
  . Union options options_ LinearProgressClassKeyOptions
  => Record options
  -> LinearProgressClassKey
linearProgressClassKey = unsafeCoerce

linearProgressClassKeyJSS :: ∀ options options_
  . Union options options_ LinearProgressClassKeyOptionsJSS
  => Record options
  -> LinearProgressClassKeyJSS
linearProgressClassKeyJSS = unsafeCoerce

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
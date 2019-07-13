module MUI.Core.LinearProgress where

import Data.Maybe (Maybe(..))
import React.Basic (JSX, ReactComponent, element)
import Simple.JSON (write)
import Unsafe.Coerce (unsafeCoerce)


type LinearProgressProps =
  ( classes :: LinearProgressClassKey
  , color :: Maybe String
  , value :: Maybe Number
  , valueBuffer :: Maybe Number
  , variant :: Maybe String
  )

linearProgressProps :: { | LinearProgressProps }
linearProgressProps = 
  { color : Just "primary"
  , classes
  , value : Nothing
  , valueBuffer : Nothing
  , variant : Just "indeterminate"
  }

type LinearProgressClassKey =
  { root :: Maybe String
  , colorPrimary :: Maybe String
  , colorSecondary :: Maybe String
  , determinate :: Maybe String
  , indeterminate :: Maybe String
  , buffer :: Maybe String
  , query :: Maybe String
  , dashed :: Maybe String
  , dashedColorPrimary :: Maybe String
  , dashedColorSecondary :: Maybe String
  , bar :: Maybe String
  , barColorPrimary :: Maybe String
  , barColorSecondary :: Maybe String
  , bar1Indeterminate :: Maybe String
  , bar1Determinate :: Maybe String
  , bar1Buffer :: Maybe String
  , bar2Indeterminate :: Maybe String
  , bar2Buffer :: Maybe String
  }

classes :: LinearProgressClassKey
classes = 
  { root : Nothing
  , colorPrimary : Nothing
  , colorSecondary : Nothing
  , determinate : Nothing
  , indeterminate : Nothing
  , buffer : Nothing
  , query : Nothing
  , dashed : Nothing
  , dashedColorPrimary : Nothing
  , dashedColorSecondary : Nothing
  , bar : Nothing
  , barColorPrimary : Nothing
  , barColorSecondary : Nothing
  , bar1Indeterminate : Nothing
  , bar1Determinate : Nothing
  , bar1Buffer : Nothing
  , bar2Indeterminate : Nothing
  , bar2Buffer : Nothing
  }

linearProgress :: { | LinearProgressProps } -> JSX
linearProgress props = do
  let foreignProps = write props
  element _LinearProgress (unsafeCoerce foreignProps)


foreign import _LinearProgress :: ∀ a. ReactComponent a
module MUI.Core.CardContent where


import MUI.Core (JSS)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import Unsafe.Coerce (unsafeCoerce)

type CardContentProps =
  ( children :: Array JSX
  , classes :: CardContentClassKey 
  , className :: String
  , component :: String
  )

foreign import data CardContentClassKey :: Type

type CardContentClassKeyOptions = ( root :: JSS )

cardContentClassKey 
  :: ∀ options options_
  . Union options options_ CardContentClassKeyOptions
  => Record options
  -> CardContentClassKey
cardContentClassKey = unsafeCoerce

cardContent
  :: ∀ props props_
  . Union props props_ CardContentProps
  => Record props 
  -> JSX
cardContent = element _CardContent


foreign import _CardContent :: ∀ a. ReactComponent a
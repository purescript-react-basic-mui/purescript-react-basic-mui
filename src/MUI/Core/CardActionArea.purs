module MUI.Core.CardActionArea where

import MUI.Core (JSS)
import MUI.Core.ButtonBase (ButtonBasePropsOptions)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_button)
import Unsafe.Coerce (unsafeCoerce)

type CardActionAreaPropsOptions componentProps = 
  ( children :: (Array JSX)
  , classes :: CardActionAreaClassKey
  | componentProps
  )

foreign import data CardActionAreaProps :: Type

type CardActionAreaClassKeyGenericOptions a =
  ( root :: a 
  , focusVisible :: a 
  , focusHighlight :: a 
  )
type CardActionAreaClassKeyOptions = CardActionAreaClassKeyGenericOptions String
type CardActionAreaClassKeyJSSOptions = CardActionAreaClassKeyGenericOptions JSS
foreign import data CardActionAreaClassKey :: Type
foreign import data CardActionAreaClassKeyJSS :: Type

cardActionAreaClassKey :: ∀  given required
  .  Union given required (CardActionAreaClassKeyOptions )
  => Record given
  -> CardActionAreaClassKey
cardActionAreaClassKey = unsafeCoerce

cardActionAreaClassKeyJSS :: ∀  given required
  .  Union given required (CardActionAreaClassKeyJSSOptions )
  => Record given
  -> CardActionAreaClassKeyJSS
cardActionAreaClassKeyJSS = unsafeCoerce

cardActionArea :: ∀  given required
  .  Union given required (CardActionAreaPropsOptions (ButtonBasePropsOptions Props_button) )
  => Record given
  -> JSX
cardActionArea = element _CardActionArea

cardActionArea_component :: ∀ componentProps given required
  .  Union given required (CardActionAreaPropsOptions componentProps)
  => Record given
  -> JSX
cardActionArea_component = element _CardActionArea

foreign import _CardActionArea :: ∀ a. ReactComponent a
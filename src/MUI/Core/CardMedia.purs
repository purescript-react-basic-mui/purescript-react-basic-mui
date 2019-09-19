module MUI.Core.CardMedia where

import MUI.Core (JSS)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_div)
import Unsafe.Coerce (unsafeCoerce)

type CardMediaPropsOptions componentProps = 
  ( classes :: CardMediaClassKey
  , component :: ReactComponent { | componentProps }
  , image :: String
  , src :: String
  | componentProps
  )

foreign import data CardMediaProps :: Type

type CardMediaClassKeyGenericOptions a =
  ( root :: a 
  , media :: a 
  )
type CardMediaClassKeyOptions = CardMediaClassKeyGenericOptions String
type CardMediaClassKeyJSSOptions = CardMediaClassKeyGenericOptions JSS
foreign import data CardMediaClassKey :: Type
foreign import data CardMediaClassKeyJSS :: Type

cardMediaClassKey :: ∀  given required
  .  Union given required (CardMediaClassKeyOptions )
  => Record given
  -> CardMediaClassKey
cardMediaClassKey = unsafeCoerce

cardMediaClassKeyJSS :: ∀  given required
  .  Union given required (CardMediaClassKeyJSSOptions )
  => Record given
  -> CardMediaClassKeyJSS
cardMediaClassKeyJSS = unsafeCoerce

cardMedia :: ∀  given required
  .  Union given required (CardMediaPropsOptions Props_div )
  => Record given
  -> JSX
cardMedia = element _CardMedia

cardMedia_component :: ∀ componentProps given required
  .  Union given required (CardMediaPropsOptions componentProps)
  => Record given
  -> JSX
cardMedia_component = element _CardMedia

foreign import _CardMedia :: ∀ a. ReactComponent a
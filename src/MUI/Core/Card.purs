module MUI.Core.Card where

import MUI.Core (JSS)
import MUI.Core.Paper (PaperProps)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_div)
import Unsafe.Coerce (unsafeCoerce)

type CardPropsOptions componentProps = 
  ( classes :: CardClassKey
  , raised :: Boolean
  | componentProps
  )

foreign import data CardProps :: Type

type CardClassKeyGenericOptions a =
  ( root :: a 
  )
type CardClassKeyOptions = CardClassKeyGenericOptions String
type CardClassKeyJSSOptions = CardClassKeyGenericOptions JSS
foreign import data CardClassKey :: Type
foreign import data CardClassKeyJSS :: Type

cardClassKey :: ∀  given required
  .  Union given required (CardClassKeyOptions )
  => Record given
  -> CardClassKey
cardClassKey = unsafeCoerce

cardClassKeyJSS :: ∀  given required
  .  Union given required (CardClassKeyJSSOptions )
  => Record given
  -> CardClassKeyJSS
cardClassKeyJSS = unsafeCoerce

card :: ∀  given required
  .  Union given required (CardPropsOptions (PaperProps Props_div) )
  => Record given
  -> JSX
card = element _Card

card_component :: ∀ componentProps given required
  .  Union given required (CardPropsOptions componentProps)
  => Record given
  -> JSX
card_component = element _Card

foreign import _Card :: ∀ a. ReactComponent a
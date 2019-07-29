module MUI.Core.Card where


 
import MUI.Core (JSS)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_div)
import Unsafe.Coerce (unsafeCoerce)

type CardProps componentProps =
  ( children :: Array JSX
  , classes :: CardClassKey 
  , component :: ReactComponent { | componentProps }
  , elevation :: Number
  , square :: Boolean
  , raised :: Boolean
  | componentProps
  )

foreign import data CardPropsPartial :: Type

foreign import data CardClassKey :: Type
foreign import data CardClassKeyJSS :: Type

type CardClassKeyOptionsJSS = CardClassKeyOptionsR JSS
type CardClassKeyOptions = CardClassKeyOptionsR String
type CardClassKeyOptionsR a =
  ( root :: a
  , rounded :: a
  , elevation0 :: a
  , elevation1 :: a
  , elevation2 :: a
  , elevation3 :: a
  , elevation4 :: a
  , elevation5 :: a
  , elevation6 :: a
  , elevation7 :: a
  , elevation8 :: a
  , elevation9 :: a
  , elevation10 :: a
  , elevation11 :: a
  , elevation12 :: a
  , elevation13 :: a
  , elevation14 :: a
  , elevation15 :: a
  , elevation16 :: a
  , elevation17 :: a
  , elevation18 :: a
  , elevation19 :: a
  , elevation20 :: a
  , elevation21 :: a
  , elevation22 :: a
  , elevation23 :: a
  , elevation24 :: a
  )

cardClassKey :: ∀ options options_
  . Union options options_ CardClassKeyOptions
  => Record options
  -> CardClassKey
cardClassKey = unsafeCoerce

cardClassKeyJSS :: ∀ options options_
  . Union options options_ CardClassKeyOptionsJSS
  => Record options
  -> CardClassKeyJSS
cardClassKeyJSS = unsafeCoerce

cardPropsPartial_component :: ∀ componentProps props props_
  . Union props props_ (CardProps componentProps)
  => Record props 
  -> CardPropsPartial 
cardPropsPartial_component = unsafeCoerce

cardPropsPartial :: ∀ props props_
  . Union props props_ (CardProps Props_div)
  => Record props 
  -> CardPropsPartial 
cardPropsPartial = unsafeCoerce


card_component :: ∀ componentProps props props_
  . Union props props_ (CardProps componentProps)
  => Record props 
  -> JSX
card_component = element _Card

card :: ∀ props props_
  . Union props props_ (CardProps Props_div)
  => Record props 
  -> JSX
card = element _Card


foreign import _Card :: ∀ a. ReactComponent a
module MUI.Core.Card where


 
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import Unsafe.Coerce (unsafeCoerce)

type CardProps =
  ( children :: Array JSX
  , classes :: CardClassKey 
  , className :: String
  , component :: String
  , elevation :: Number
  , square :: Boolean
  , raised :: Boolean
  )

foreign import data CardPropsPartial :: Type

foreign import data CardClassKey :: Type

type CardClassKeyOptions =
  ( root :: String
  , rounded :: String
  , elevation0 :: String
  , elevation1 :: String
  , elevation2 :: String
  , elevation3 :: String
  , elevation4 :: String
  , elevation5 :: String
  , elevation6 :: String
  , elevation7 :: String
  , elevation8 :: String
  , elevation9 :: String
  , elevation10 :: String
  , elevation11 :: String
  , elevation12 :: String
  , elevation13 :: String
  , elevation14 :: String
  , elevation15 :: String
  , elevation16 :: String
  , elevation17 :: String
  , elevation18 :: String
  , elevation19 :: String
  , elevation20 :: String
  , elevation21 :: String
  , elevation22 :: String
  , elevation23 :: String
  , elevation24 :: String
  )

cardClassKey :: ∀ options options_
  . Union options options_ CardClassKeyOptions
  => Record options
  -> CardClassKey
cardClassKey = unsafeCoerce

cardPropsPartial :: ∀ props props_
  . Union props props_ CardProps
  => Record props 
  -> CardPropsPartial 
cardPropsPartial = unsafeCoerce

card :: ∀ props props_
  . Union props props_ CardProps
  => Record props 
  -> JSX
card = element _Card


foreign import _Card :: ∀ a. ReactComponent a
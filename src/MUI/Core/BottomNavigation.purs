module MUI.Core.BottomNavigation where

import Prelude

import Effect.Uncurried (EffectFn2)
import MUI.Core (JSS)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_div)
import React.Basic.Events (SyntheticEvent)
import Unsafe.Coerce (unsafeCoerce)

type BottomNavigationPropsOptions componentProps value = 
  ( children :: (Array JSX)
  , classes :: BottomNavigationClassKey
  , component :: ReactComponent { | componentProps }
  , onChange :: EffectFn2 SyntheticEvent value Unit
  , showLabels :: Boolean
  , value :: value
  | componentProps
  )

foreign import data BottomNavigationProps :: Type

type BottomNavigationClassKeyGenericOptions a =
  ( root :: a 
  )
type BottomNavigationClassKeyOptions = BottomNavigationClassKeyGenericOptions String
type BottomNavigationClassKeyJSSOptions = BottomNavigationClassKeyGenericOptions JSS
foreign import data BottomNavigationClassKey :: Type
foreign import data BottomNavigationClassKeyJSS :: Type

bottomNavigationClassKey :: ∀  given required
  .  Union given required (BottomNavigationClassKeyOptions )
  => Record given
  -> BottomNavigationClassKey
bottomNavigationClassKey = unsafeCoerce

bottomNavigationClassKeyJSS :: ∀  given required
  .  Union given required (BottomNavigationClassKeyJSSOptions )
  => Record given
  -> BottomNavigationClassKeyJSS
bottomNavigationClassKeyJSS = unsafeCoerce

bottomNavigation :: ∀ value given required
  .  Union given required (BottomNavigationPropsOptions Props_div value)
  => Record given
  -> JSX
bottomNavigation = element _BottomNavigation

bottomNavigation_component :: ∀ componentProps value given required
  .  Union given required (BottomNavigationPropsOptions componentProps value)
  => Record given
  -> JSX
bottomNavigation_component = element _BottomNavigation

foreign import _BottomNavigation :: ∀ a. ReactComponent a
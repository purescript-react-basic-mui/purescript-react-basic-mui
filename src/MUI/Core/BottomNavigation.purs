module MUI.Core.BottomNavigation where

import Prelude

import Effect.Uncurried (EffectFn2)
import MUI.Core (JSS)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_div)
import React.Basic.Events (SyntheticEvent)
import Unsafe.Coerce (unsafeCoerce)

type BottomNavigationProps value componentProps =
  ( children :: Array JSX
  , classes :: BottomNavigationClassKey
  , component :: ReactComponent { | componentProps }
  , onChange :: EffectFn2 SyntheticEvent value Unit
  , showLabels :: Boolean
  , value :: value
  | componentProps
  )

foreign import data BottomNavigationPropsPartial :: Type

foreign import data BottomNavigationClassKey :: Type
foreign import data BottomNavigationClassKeyJSS :: Type
type BottomNavigationClassKeyOptions = BottomNavigationClassKeyOptionsR String
type BottomNavigationClassKeyOptionsJSS = BottomNavigationClassKeyOptionsR JSS 
type BottomNavigationClassKeyOptionsR a = ( root :: a )

bottomNavigationClassKey :: ∀ options options_
  . Union options options_ BottomNavigationClassKeyOptions
  => Record options
  -> BottomNavigationClassKey
bottomNavigationClassKey = unsafeCoerce

bottomNavigationClassKeyJSS :: ∀ options options_
  . Union options options_ BottomNavigationClassKeyOptionsJSS
  => Record options
  -> BottomNavigationClassKeyJSS
bottomNavigationClassKeyJSS = unsafeCoerce

bottomNavigationPropsPartial_component :: ∀ value componentProps props props_
  . Union props props_ (BottomNavigationProps value componentProps)
  => Record props 
  -> BottomNavigationPropsPartial
bottomNavigationPropsPartial_component = unsafeCoerce

bottomNavigationPropsPartial :: ∀ value props props_
  . Union props props_ (BottomNavigationProps value Props_div)
  => Record props 
  -> BottomNavigationPropsPartial
bottomNavigationPropsPartial = unsafeCoerce

bottomNavigation_component :: ∀ value componentProps props props_
  . Union props props_ (BottomNavigationProps value componentProps)
  => Record props 
  -> JSX
bottomNavigation_component = element _BottomNavigation

bottomNavigation :: ∀ value props props_
  . Union props props_ (BottomNavigationProps value Props_div)
  => Record props 
  -> JSX
bottomNavigation = element _BottomNavigation


foreign import _BottomNavigation :: ∀ a. ReactComponent a
module MUI.Core.BottomNavigationAction where

import MUI.Core (JSS)
import MUI.Core.ButtonBase (ButtonBasePropsOptions)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_button)
import Unsafe.Coerce (unsafeCoerce)

type BottomNavigationActionPropsOptions componentProps value = 
  ( classes :: BottomNavigationActionClassKey
  , icon :: JSX
  , label :: JSX
  , showLabel :: Boolean
  , value :: value
  | componentProps
  )

foreign import data BottomNavigationActionProps :: Type

type BottomNavigationActionClassKeyGenericOptions a =
  ( root :: a 
  , selected :: a 
  , iconOnly :: a 
  , wrapper :: a 
  , label :: a 
  )
type BottomNavigationActionClassKeyOptions = BottomNavigationActionClassKeyGenericOptions String
type BottomNavigationActionClassKeyJSSOptions = BottomNavigationActionClassKeyGenericOptions JSS
foreign import data BottomNavigationActionClassKey :: Type
foreign import data BottomNavigationActionClassKeyJSS :: Type

bottomNavigationActionClassKey :: ∀  given required
  .  Union given required (BottomNavigationActionClassKeyOptions )
  => Record given
  -> BottomNavigationActionClassKey
bottomNavigationActionClassKey = unsafeCoerce

bottomNavigationActionClassKeyJSS :: ∀  given required
  .  Union given required (BottomNavigationActionClassKeyJSSOptions )
  => Record given
  -> BottomNavigationActionClassKeyJSS
bottomNavigationActionClassKeyJSS = unsafeCoerce

bottomNavigationAction :: ∀ value given required
  .  Union given required (BottomNavigationActionPropsOptions (ButtonBasePropsOptions Props_button) value)
  => Record given
  -> JSX
bottomNavigationAction = element _BottomNavigationAction

bottomNavigationAction_component :: ∀ componentProps value given required
  .  Union given required (BottomNavigationActionPropsOptions componentProps value)
  => Record given
  -> JSX
bottomNavigationAction_component = element _BottomNavigationAction

foreign import _BottomNavigationAction :: ∀ a. ReactComponent a
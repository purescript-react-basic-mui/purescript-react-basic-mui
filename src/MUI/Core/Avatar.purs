module MUI.Core.Avatar where

import Foreign.Object (Object)
import MUI.Core (JSS)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_div)
import Unsafe.Coerce (unsafeCoerce)

type AvatarProps componentProps =
  ( alt :: String
  , children :: Array JSX
  , classes :: AvatarClassKey
  , component :: ReactComponent { | componentProps }
  , imgProps :: Object String
  , sizes :: String
  , src :: String
  , srcSet :: String
  | componentProps
  )

foreign import data AvatarPropsPartial :: Type


type AvatarClassKeyOptions = AvatarClassKeyOptionsR String
type AvatarClassKeyOptionsJSS = AvatarClassKeyOptionsR JSS 
type AvatarClassKeyOptionsR a =
  ( root :: a
  , colorDefault :: a
  , img :: a
  )

foreign import data AvatarClassKey :: Type
foreign import data AvatarClassKeyJSS :: Type

avatarClassKey :: ∀ options options_
  . Union options options_ AvatarClassKeyOptions
  => Record options
  -> AvatarClassKey
avatarClassKey = unsafeCoerce

avatarClassKeyJSS :: ∀ options options_
  . Union options options_ AvatarClassKeyOptionsJSS
  => Record options
  -> AvatarClassKeyJSS
avatarClassKeyJSS = unsafeCoerce

avatarPropsPartial_component :: ∀ componentProps props props_
  . Union props props_ (AvatarProps componentProps)
  => Record props 
  -> AvatarPropsPartial
avatarPropsPartial_component = unsafeCoerce

avatarPropsPartial :: ∀ props props_
  . Union props props_ (AvatarProps Props_div)
  => Record props 
  -> AvatarPropsPartial
avatarPropsPartial = unsafeCoerce

avatar_component :: ∀ componentProps props props_
  . Union props props_ (AvatarProps componentProps)
  => Record props 
  -> JSX
avatar_component = element _Avatar

avatar :: ∀ props props_
  . Union props props_ (AvatarProps Props_div)
  => Record props 
  -> JSX
avatar = element _Avatar

foreign import _Avatar :: ∀ a. ReactComponent a

module MUI.Core.Avatar where

import MUI.Core (JSS)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_div, Props_img)
import Unsafe.Coerce (unsafeCoerce)

type AvatarPropsOptions componentProps = 
  ( alt :: String
  , children :: (Array JSX)
  , classes :: AvatarClassKey
  , component :: ReactComponent { | componentProps }
  , imgProps :: { | Props_img }
  , sizes :: String
  , src :: String
  , srcSet :: String
  | componentProps
  )

foreign import data AvatarProps :: Type



type AvatarClassKeyGenericOptions a =
  ( root :: a 
  , colorDefault :: a 
  , img :: a 
  )
type AvatarClassKeyOptions = AvatarClassKeyGenericOptions String
type AvatarClassKeyJSSOptions = AvatarClassKeyGenericOptions JSS
foreign import data AvatarClassKey :: Type
foreign import data AvatarClassKeyJSS :: Type

avatarClassKey :: ∀  given required
  .  Union given required (AvatarClassKeyOptions )
  => Record given
  -> AvatarClassKey
avatarClassKey = unsafeCoerce

avatarClassKeyJSS :: ∀  given required
  .  Union given required (AvatarClassKeyJSSOptions )
  => Record given
  -> AvatarClassKeyJSS
avatarClassKeyJSS = unsafeCoerce

avatar :: ∀  given required
  .  Union given required (AvatarPropsOptions Props_div )
  => Record given
  -> JSX
avatar = element _Avatar

avatar_component :: ∀ componentProps given required
  .  Union given required (AvatarPropsOptions componentProps)
  => Record given
  -> JSX
avatar_component = element _Avatar

foreign import _Avatar :: ∀ a. ReactComponent a
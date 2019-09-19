module MUI.Core.Chip where

import MUI.Core (JSS)
import MUI.Core.Chip.Color (ColorProp)
import MUI.Core.Chip.Size (SizeProp)
import MUI.Core.Chip.Variant (VariantProp)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_div)
import React.Basic.Events (EventHandler)
import Unsafe.Coerce (unsafeCoerce)

type ChipPropsOptions componentProps = 
  ( avatar :: JSX
  , children :: (Array JSX)
  , classes :: ChipClassKey
  , clickable :: Boolean
  , color :: ColorProp
  , component :: ReactComponent { | componentProps }
  , deleteIcon :: JSX
  , icon :: JSX
  , label :: JSX
  , onDelete :: EventHandler
  , size :: SizeProp
  , variant :: VariantProp
  | componentProps
  )

foreign import data ChipProps :: Type

type ChipClassKeyGenericOptions a =
  ( root :: a 
  , sizeSmall :: a 
  , colorPrimary :: a 
  , colorSecondary :: a 
  , clickable :: a 
  , clickableColorPrimary :: a 
  , clickableColorSecondary :: a 
  , deletable :: a 
  , deletableColorPrimary :: a 
  , deletableColorSecondary :: a 
  , outlined :: a 
  , outlinedPrimary :: a 
  , outlinedSecondary :: a 
  , avatar :: a 
  , avatarSmall :: a 
  , avatarColorPrimary :: a 
  , avatarColorSecondary :: a 
  , avatarChildren :: a 
  , icon :: a 
  , iconSmall :: a 
  , iconColorPrimary :: a 
  , iconColorSecondary :: a 
  , label :: a 
  , labelSmall :: a 
  , deleteIcon :: a 
  , deleteIconSmall :: a 
  , deleteIconColorPrimary :: a 
  , deleteIconColorSecondary :: a 
  , deleteIconOutlinedColorPrimary :: a 
  , deleteIconOutlinedColorSecondary :: a 
  )
type ChipClassKeyOptions = ChipClassKeyGenericOptions String
type ChipClassKeyJSSOptions = ChipClassKeyGenericOptions JSS
foreign import data ChipClassKey :: Type
foreign import data ChipClassKeyJSS :: Type

chipClassKey :: ∀  given required
  .  Union given required (ChipClassKeyOptions )
  => Record given
  -> ChipClassKey
chipClassKey = unsafeCoerce

chipClassKeyJSS :: ∀  given required
  .  Union given required (ChipClassKeyJSSOptions )
  => Record given
  -> ChipClassKeyJSS
chipClassKeyJSS = unsafeCoerce

chip :: ∀  given required
  .  Union given required (ChipPropsOptions Props_div )
  => Record given
  -> JSX
chip = element _Chip

chip_component :: ∀ componentProps given required
  .  Union given required (ChipPropsOptions componentProps)
  => Record given
  -> JSX
chip_component = element _Chip

foreign import _Chip :: ∀ a. ReactComponent a
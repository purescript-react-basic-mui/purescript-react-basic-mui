module React.Basic.MUI.Core.CardMedia where 

import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic.MUI.Core.OverridableComponent (OverrideProps)

type CardMediaTypeMap_required  p d optional =
  ( props :: Foreign
  , defaultComponent :: Foreign
  , classKey :: Foreign
  | optional )

type CardMediaTypeMap_optional p d =
  ( 
  )

foreign import data CardMediaTypeMap :: Type  -> Type



cardMedia :: Foreign
cardMedia = _CardMedia
foreign import _CardMedia :: Foreign

type CardMediaClassKey = Foreign

type CardMediaProps d p = OverrideProps CardMediaTypeMap Foreign Foreign Foreign
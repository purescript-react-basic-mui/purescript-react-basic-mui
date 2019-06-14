module React.Basic.MUI.CardMedia where 

import Prelude
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic (element, ReactComponent, ReactComponent)

type CardMediaTypeMap_required optional p d =
  ( props :: Foreign
  , defaultComponent :: Foreign
  , classKey :: Foreign
  | optional )

type CardMediaTypeMap_optional p d =
  ( 
  )

foreign import data CardMediaTypeMap :: Type Type

cardMediaTypeMap
  :: ∀ attrs attrs_
   . Union attrs attrs_ (CardMediaTypeMap_optional)
  => Record (CardMediaTypeMap_required attrs)
  -> CardMediaTypeMap
cardMediaTypeMap = unsafeCoerce

cardMedia :: Foreign
cardMedia = _CardMedia
foreign import _CardMedia :: Foreign

type CardMediaClassKey = Foreign

type CardMediaProps d p = OverrideProps CardMediaTypeMap Foreign Foreign Foreign
module React.Basic.MUI.OverridableComponent where 

import Prelude
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic.DOM.Internal (CSS)
import React.Basic (element, ReactComponent, JSX)

type OverridableComponent_optional m =
  ( 
  )

foreign import data OverridableComponent :: Type 

overridableComponent
  :: ∀ attrs attrs_
   . Union attrs attrs_ (OverridableComponent_optional)
  => Record (attrs)
  -> OverridableComponent
overridableComponent = unsafeCoerce

type OverrideProps m c = (Foreign)

type DefaultComponentProps m = Foreign

type BaseProps m = Foreign

type CommonProps_optional m =
  ( className :: String
  , style :: CSS
  , classes :: Foreign
  , innerRef :: Foreign
  )

foreign import data CommonProps :: Type 

commonProps
  :: ∀ attrs attrs_
   . Union attrs attrs_ (CommonProps_optional)
  => Record (attrs)
  -> CommonProps
commonProps = unsafeCoerce

type OverridableTypeMap_required optional =
  ( props :: {  }
  , defaultComponent :: JSX
  , classKey :: String
  | optional )

type OverridableTypeMap_optional =
  ( 
  )

foreign import data OverridableTypeMap :: Type 

overridableTypeMap
  :: ∀ attrs attrs_
   . Union attrs attrs_ (OverridableTypeMap_optional)
  => Record (OverridableTypeMap_required attrs)
  -> OverridableTypeMap
overridableTypeMap = unsafeCoerce

type Simplify t = Foreign

type SimplifiedPropsOf c = Simplify Foreign
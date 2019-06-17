module React.Basic.MUI.Core.OverridableComponent where 

import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic.DOM.Internal (CSS)
import React.Basic (element, ReactComponent, JSX)

type OverridableComponent_optional m =
  ( 
  )

foreign import data OverridableComponent :: Type 



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
  :: ∀ m attrs attrs_
   . Union attrs attrs_ (CommonProps_optional m )
  => Record (attrs)
  -> CommonProps
commonProps = unsafeCoerce

type OverridableTypeMap_required  optional =
  ( props :: {  }
  , defaultComponent :: JSX
  , classKey :: String
  | optional )

type OverridableTypeMap_optional =
  ( 
  )

foreign import data OverridableTypeMap :: Type 



type Simplify t = Foreign

type SimplifiedPropsOf c = Simplify Foreign
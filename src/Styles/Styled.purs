module React.Basic.MUI.Styles.Styled where 

import Prelude
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic (element, ReactComponent, ReactComponent)

type ComponentCreator component = Foreign

type StyledProps_required optional =
  ( className :: String
  | optional )

type StyledProps_optional =
  ( 
  )

foreign import data StyledProps :: Type 

styledProps
  :: ∀ attrs attrs_
   . Union attrs attrs_ (StyledProps_optional)
  => Record (StyledProps_required attrs)
  -> StyledProps
styledProps = unsafeCoerce

styled :: Foreign
styled = _styled
foreign import _styled :: Foreign
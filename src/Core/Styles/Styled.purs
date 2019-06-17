module React.Basic.MUI.Core.Styles.Styled where 

import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic (element, ReactComponent, JSX)
import React.Basic.DOM.Internal (CSS)
import React.Basic.MUI.Core.Styles.CreateMuiTheme (Theme)

type ComponentCreator component = Foreign

type StyledProps_required  optional =
  ( className :: String
  | optional )

type StyledProps_optional =
  ( 
  )

foreign import data StyledProps :: Type 

styledProps
  :: ∀ attrs attrs_
   . Union attrs attrs_ (StyledProps_optional )
  => Record (StyledProps_required attrs)
  -> StyledProps
styledProps = unsafeCoerce

styled :: Foreign
styled = _styled
foreign import _styled :: Foreign
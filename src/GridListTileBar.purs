module React.Basic.MUI.GridListTileBar where 

import Prelude
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic (element, ReactComponent, JSX)
import React.Basic.DOM.Internal (CSS)

type GridListTileBarProps_optional =
  ( actionIcon :: JSX
  , actionPosition :: Foreign
  , subtitle :: JSX
  , title :: JSX
  , titlePosition :: Foreign
  , classes :: Foreign
  , innerRef :: Foreign
  , className :: String
  , style :: CSS
  , ref :: Foreign
  )

foreign import data GridListTileBarProps :: Type 

gridListTileBarProps
  :: ∀ attrs attrs_
   . Union attrs attrs_ (GridListTileBarProps_optional)
  => Record (attrs)
  -> GridListTileBarProps
gridListTileBarProps = unsafeCoerce

type GridListTileBarClassKey = Foreign

gridListTileBar
  :: ∀ attrs attrs_
   . Union attrs attrs_ (GridListTileBarProps_optional)
  => Record (attrs)
  -> JSX
gridListTileBar = element _GridListTileBar
foreign import _GridListTileBar :: forall a. ReactComponent a 
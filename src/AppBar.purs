-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/AppBar/AppBar.d.ts
module MaterialUI.Basic.AppBar where 
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)

import MaterialUI.Basic.Index (Color)




foreign import _appBar :: forall a. ReactComponent a



type AppBarProps  = 
  ( color :: Color
  ,  position :: String
  ,  key :: String
  ,  children :: Array JSX
  )

appBar
  :: forall attrs attrs_  
  . Union attrs attrs_ (AppBarProps  )
  => Record attrs
  -> JSX
appBar props = element _appBar props
 

appBar_ :: Array JSX -> JSX
appBar_ children = appBar { children }  

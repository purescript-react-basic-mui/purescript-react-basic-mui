-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/Collapse/Collapse.d.ts
module MaterialUI.Basic.Collapse where 
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)

import MaterialUI.Basic.Styles.CreateMuiTheme (Theme)


foreign import data CollapsePropsTimeout :: Type

foreign import _collapse :: forall a. ReactComponent a



type CollapseProps  = 
  ( collapsedHeight :: String
  ,  component :: JSX
  ,  theme :: Theme
  ,  timeout :: CollapsePropsTimeout
  ,  key :: String
  ,  children :: Array JSX
  )

collapse
  :: forall attrs attrs_  
  . Union attrs attrs_ (CollapseProps  )
  => Record attrs
  -> JSX
collapse props = element _collapse props
 

collapse_ :: Array JSX -> JSX
collapse_ children = collapse { children }  

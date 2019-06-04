-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/Grow/Grow.d.ts
module MaterialUI.Basic.Grow where 
import Foreign (Foreign)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, Ref, element)

import MaterialUI.Basic.Styles.CreateMuiTheme (Theme)


foreign import data GrowPropsTimeout :: Type

foreign import _grow :: forall a. ReactComponent a



type GrowProps  = 
  ( ref :: (Ref Foreign)
  ,  theme :: Theme
  ,  timeout :: GrowPropsTimeout
  ,  key :: String
  ,  children :: Array JSX
  )

grow
  :: forall attrs attrs_  
  . Union attrs attrs_ (GrowProps  )
  => Record attrs
  -> JSX
grow props = element _grow props
 

grow_ :: Array JSX -> JSX
grow_ children = grow { children }  

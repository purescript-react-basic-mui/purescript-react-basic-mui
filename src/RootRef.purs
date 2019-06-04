-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/RootRef/RootRef.d.ts
module MaterialUI.Basic.RootRef where 
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)



foreign import data RootRefPropsRootRef :: Type

foreign import _rootRef :: forall a. ReactComponent a



type RootRefProps t = 
  ( rootRef :: RootRefPropsRootRef
  ,  key :: String
  ,  children :: Array JSX
  )

rootRef
  :: forall attrs attrs_ t 
  . Union attrs attrs_ (RootRefProps t )
  => Record attrs
  -> JSX
rootRef props = element _rootRef props
 

rootRef_ :: Array JSX -> JSX
rootRef_ children = rootRef { children }  

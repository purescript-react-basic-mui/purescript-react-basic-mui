-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/Container/Container.d.ts
module MaterialUI.Basic.Container where 
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)



foreign import data ContainerPropsMaxWidth :: Type

foreign import _container :: forall a. ReactComponent a



type ContainerProps  = 
  ( component :: JSX
  ,  fixed :: Boolean
  ,  maxWidth :: ContainerPropsMaxWidth
  ,  key :: String
  ,  children :: Array JSX
  )

container
  :: forall attrs attrs_  
  . Union attrs attrs_ (ContainerProps  )
  => Record attrs
  -> JSX
container props = element _container props
 

container_ :: Array JSX -> JSX
container_ children = container { children }  

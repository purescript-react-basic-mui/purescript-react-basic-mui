-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/Hidden/Hidden.d.ts
module MaterialUI.Basic.Hidden where 
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)





foreign import _hidden :: forall a. ReactComponent a



type HiddenProps  = 
  ( implementation :: String
  ,  initialWidth :: String
  ,  lgDown :: Boolean
  ,  lgUp :: Boolean
  ,  mdDown :: Boolean
  ,  mdUp :: Boolean
  ,  only :: (Array String)
  ,  smDown :: Boolean
  ,  smUp :: Boolean
  ,  xlDown :: Boolean
  ,  xlUp :: Boolean
  ,  xsDown :: Boolean
  ,  xsUp :: Boolean
  ,  key :: String
  ,  children :: Array JSX
  )

hidden
  :: forall attrs attrs_  
  . Union attrs attrs_ (HiddenProps  )
  => Record attrs
  -> JSX
hidden props = element _hidden props
 

hidden_ :: Array JSX -> JSX
hidden_ children = hidden { children }  

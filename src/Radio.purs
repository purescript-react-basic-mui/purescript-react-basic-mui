-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/Radio/Radio.d.ts
module MaterialUI.Basic.Radio where 
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)





foreign import _radio :: forall a. ReactComponent a



type RadioProps  = 
  ( checkedIcon :: JSX
  ,  color :: String
  ,  icon :: JSX
  ,  key :: String
  ,  children :: Array JSX
  )

radio
  :: forall attrs attrs_  
  . Union attrs attrs_ (RadioProps  )
  => Record attrs
  -> JSX
radio props = element _radio props
 

radio_ :: Array JSX -> JSX
radio_ children = radio { children }  

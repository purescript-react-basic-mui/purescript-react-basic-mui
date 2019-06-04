-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/Box/Box.d.ts
module MaterialUI.Basic.Box where 
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)





foreign import _box :: forall a. ReactComponent a



type BoxProps  = 
  ( bgcolor :: String
  ,  clone :: Boolean
  ,  component :: JSX
  ,  displayPrint :: String
  ,  m :: String
  ,  mb :: String
  ,  ml :: String
  ,  mr :: String
  ,  mt :: String
  ,  mx :: String
  ,  my :: String
  ,  order :: String
  ,  p :: String
  ,  pb :: String
  ,  pl :: String
  ,  pr :: String
  ,  pt :: String
  ,  px :: String
  ,  py :: String
  ,  key :: String
  ,  children :: Array JSX
  )

box
  :: forall attrs attrs_  
  . Union attrs attrs_ (BoxProps  )
  => Record attrs
  -> JSX
box props = element _box props
 

box_ :: Array JSX -> JSX
box_ children = box { children }  

-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/Card/Card.d.ts
module MaterialUI.Basic.Card where 
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)





foreign import _card :: forall a. ReactComponent a



type CardProps  = 
  ( raised :: Boolean
  ,  key :: String
  ,  children :: Array JSX
  )

card
  :: forall attrs attrs_  
  . Union attrs attrs_ (CardProps  )
  => Record attrs
  -> JSX
card props = element _card props
 

card_ :: Array JSX -> JSX
card_ children = card { children }  

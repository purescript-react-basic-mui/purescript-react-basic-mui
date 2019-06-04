-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/CardContent/CardContent.d.ts
module MaterialUI.Basic.CardContent where 
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)





foreign import _cardContent :: forall a. ReactComponent a



type CardContentProps  = 
  ( component :: JSX
  ,  key :: String
  ,  children :: Array JSX
  )

cardContent
  :: forall attrs attrs_  
  . Union attrs attrs_ (CardContentProps  )
  => Record attrs
  -> JSX
cardContent props = element _cardContent props
 

cardContent_ :: Array JSX -> JSX
cardContent_ children = cardContent { children }  

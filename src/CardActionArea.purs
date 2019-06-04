-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/CardActionArea/CardActionArea.d.ts
module MaterialUI.Basic.CardActionArea where 
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)





foreign import _cardActionArea :: forall a. ReactComponent a



type CardActionAreaProps  = 
  ( focusVisibleClassName :: String
  ,  key :: String
  ,  children :: Array JSX
  )

cardActionArea
  :: forall attrs attrs_  
  . Union attrs attrs_ (CardActionAreaProps  )
  => Record attrs
  -> JSX
cardActionArea props = element _cardActionArea props
 

cardActionArea_ :: Array JSX -> JSX
cardActionArea_ children = cardActionArea { children }  

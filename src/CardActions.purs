-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/CardActions/CardActions.d.ts
module MaterialUI.Basic.CardActions where 
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)





foreign import _cardActions :: forall a. ReactComponent a



type CardActionsProps  = 
  ( disableSpacing :: Boolean
  ,  key :: String
  ,  children :: Array JSX
  )

cardActions
  :: forall attrs attrs_  
  . Union attrs attrs_ (CardActionsProps  )
  => Record attrs
  -> JSX
cardActions props = element _cardActions props
 

cardActions_ :: Array JSX -> JSX
cardActions_ children = cardActions { children }  

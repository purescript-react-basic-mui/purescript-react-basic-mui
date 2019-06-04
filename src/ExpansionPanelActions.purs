-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/ExpansionPanelActions/ExpansionPanelActions.d.ts
module MaterialUI.Basic.ExpansionPanelActions where 
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)





foreign import _expansionPanelActions :: forall a. ReactComponent a



type ExpansionPanelActionsProps  = 
  ( key :: String
  ,  children :: Array JSX
  )

expansionPanelActions
  :: forall attrs attrs_  
  . Union attrs attrs_ (ExpansionPanelActionsProps  )
  => Record attrs
  -> JSX
expansionPanelActions props = element _expansionPanelActions props
 

expansionPanelActions_ :: Array JSX -> JSX
expansionPanelActions_ children = expansionPanelActions { children }  

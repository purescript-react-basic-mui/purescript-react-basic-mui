-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/ExpansionPanelDetails/ExpansionPanelDetails.d.ts
module MaterialUI.Basic.ExpansionPanelDetails where 
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)





foreign import _expansionPanelDetails :: forall a. ReactComponent a



type ExpansionPanelDetailsProps  = 
  ( key :: String
  ,  children :: Array JSX
  )

expansionPanelDetails
  :: forall attrs attrs_  
  . Union attrs attrs_ (ExpansionPanelDetailsProps  )
  => Record attrs
  -> JSX
expansionPanelDetails props = element _expansionPanelDetails props
 

expansionPanelDetails_ :: Array JSX -> JSX
expansionPanelDetails_ children = expansionPanelDetails { children }  

-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/ExpansionPanelSummary/ExpansionPanelSummary.d.ts
module MaterialUI.Basic.ExpansionPanelSummary where 
import Foreign (Foreign)
import Prim.Row (class Union)
import React.Basic.Events (EventHandler)
import React.Basic (JSX, ReactComponent, element)





foreign import _expansionPanelSummary :: forall a. ReactComponent a



type ExpansionPanelSummaryProps  = 
  ( "IconButtonProps" :: Foreign
  ,  disabled :: Boolean
  ,  expandIcon :: JSX
  ,  expanded :: Boolean
  ,  onChange :: EventHandler
  ,  key :: String
  ,  children :: Array JSX
  )

expansionPanelSummary
  :: forall attrs attrs_  
  . Union attrs attrs_ (ExpansionPanelSummaryProps  )
  => Record attrs
  -> JSX
expansionPanelSummary props = element _expansionPanelSummary props
 

expansionPanelSummary_ :: Array JSX -> JSX
expansionPanelSummary_ children = expansionPanelSummary { children }  

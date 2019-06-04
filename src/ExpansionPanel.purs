-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/ExpansionPanel/ExpansionPanel.d.ts
module MaterialUI.Basic.ExpansionPanel where 
import Effect.Uncurried (EffectFn2)
import Foreign (Foreign)
import Prelude
import Prim.Row (class Union)
import React.Basic (Component, JSX, ReactComponent, element)





foreign import _expansionPanel :: forall a. ReactComponent a



type ExpansionPanelProps  = 
  ( "TransitionComponent" :: (Component Foreign)
  ,  "TransitionProps" :: Foreign
  ,  defaultExpanded :: Boolean
  ,  disabled :: Boolean
  ,  expanded :: Boolean
  ,  onChange :: (EffectFn2 Foreign Boolean Unit)
  ,  key :: String
  ,  children :: Array JSX
  )

expansionPanel
  :: forall attrs attrs_  
  . Union attrs attrs_ (ExpansionPanelProps  )
  => Record attrs
  -> JSX
expansionPanel props = element _expansionPanel props
 

expansionPanel_ :: Array JSX -> JSX
expansionPanel_ children = expansionPanel { children }  

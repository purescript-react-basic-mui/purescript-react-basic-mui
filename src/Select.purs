-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/Select/Select.d.ts
module MaterialUI.Basic.Select where 
import Foreign (Foreign)
import Prim.Row (class Union)
import React.Basic.Events (EventHandler)
import React.Basic (JSX, ReactComponent, element)





foreign import _select :: forall a. ReactComponent a



type SelectProps  = 
  ( "IconComponent" :: JSX
  ,  "MenuProps" :: Foreign
  ,  "SelectDisplayProps" :: Foreign
  ,  autoWidth :: Boolean
  ,  displayEmpty :: Boolean
  ,  input :: JSX
  ,  multiple :: Boolean
  ,  native :: Boolean
  ,  onClose :: EventHandler
  ,  onOpen :: EventHandler
  ,  open :: Boolean
  ,  renderValue :: EventHandler
  ,  value :: Foreign
  ,  variant :: String
  ,  key :: String
  ,  children :: Array JSX
  )

select
  :: forall attrs attrs_  
  . Union attrs attrs_ (SelectProps  )
  => Record attrs
  -> JSX
select props = element _select props
 

select_ :: Array JSX -> JSX
select_ children = select { children }  

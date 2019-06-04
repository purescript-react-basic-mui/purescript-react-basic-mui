-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/Select/SelectInput.d.ts
module MaterialUI.Basic.SelectInput where 
import Effect.Uncurried (EffectFn2)
import Foreign (Foreign)
import Prelude
import Prim.Row (class Union)
import React.Basic.Events (EventHandler)
import React.Basic (JSX, ReactComponent, element)





foreign import _selectInput :: forall a. ReactComponent a



type SelectInputProps_optional  = 
  ( "IconComponent" :: JSX
  ,  "MenuProps" :: Foreign
  ,  "SelectDisplayProps" :: Foreign
  ,  autoFocus :: Boolean
  ,  disabled :: Boolean
  ,  inputRef :: EventHandler
  ,  name :: String
  ,  onBlur :: Foreign
  ,  onChange :: (EffectFn2 Foreign JSX Unit)
  ,  onClose :: EventHandler
  ,  onFocus :: Foreign
  ,  onOpen :: EventHandler
  ,  open :: Boolean
  ,  readOnly :: Boolean
  ,  renderValue :: EventHandler
  ,  tabIndex :: Number
  ,  variant :: String
  ,  key :: String
  ,  children :: Array JSX
  )



type SelectInputProps_required   optional = 
  ( autoWidth :: Boolean
  ,  multiple :: Boolean
  ,  native :: Boolean
  ,  value :: Foreign
  | optional
  )

selectInput
  :: forall attrs attrs_  
  . Union attrs attrs_ (SelectInputProps_optional  )
  => Record ((SelectInputProps_required  ) attrs)
  -> JSX
selectInput props = element _selectInput props  

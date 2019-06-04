-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/FormControlLabel/FormControlLabel.d.ts
module MaterialUI.Basic.FormControlLabel where 
import Effect.Uncurried (EffectFn2)
import Foreign (Foreign)
import Prelude
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, Ref, element)





foreign import _formControlLabel :: forall a. ReactComponent a



type FormControlLabelProps_optional  = 
  ( checked :: Boolean
  ,  disabled :: Boolean
  ,  inputRef :: (Ref Foreign)
  ,  labelPlacement :: String
  ,  name :: String
  ,  onChange :: (EffectFn2 Foreign Boolean Unit)
  ,  value :: Foreign
  ,  key :: String
  ,  children :: Array JSX
  )



type FormControlLabelProps_required   optional = 
  ( control :: JSX
  ,  label :: JSX
  | optional
  )

formControlLabel
  :: forall attrs attrs_  
  . Union attrs attrs_ (FormControlLabelProps_optional  )
  => Record ((FormControlLabelProps_required  ) attrs)
  -> JSX
formControlLabel props = element _formControlLabel props  

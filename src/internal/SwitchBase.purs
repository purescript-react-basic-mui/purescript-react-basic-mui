-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/internal/SwitchBase.d.ts
module MaterialUI.Basic.Internal.SwitchBase where 
import Effect.Uncurried (EffectFn2)
import Foreign (Foreign)
import Prelude
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, Ref, element)





foreign import _switchBase :: forall a. ReactComponent a



type SwitchBaseProps_optional  = 
  ( autoFocus :: Boolean
  ,  checked :: Boolean
  ,  defaultChecked :: Boolean
  ,  disableRipple :: Boolean
  ,  disabled :: Boolean
  ,  inputProps :: Foreign
  ,  inputRef :: (Ref Foreign)
  ,  name :: String
  ,  onChange :: (EffectFn2 Foreign Boolean Unit)
  ,  readOnly :: Boolean
  ,  required :: Boolean
  ,  tabIndex :: Number
  ,  value :: Foreign
  ,  key :: String
  ,  children :: Array JSX
  )



type SwitchBaseProps_required   optional = 
  ( checkedIcon :: JSX
  ,  icon :: JSX
  | optional
  )

switchBase
  :: forall attrs attrs_  
  . Union attrs attrs_ (SwitchBaseProps_optional  )
  => Record ((SwitchBaseProps_required  ) attrs)
  -> JSX
switchBase props = element _switchBase props  

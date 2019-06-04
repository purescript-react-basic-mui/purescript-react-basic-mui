-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/InputBase/InputBase.d.ts
module MaterialUI.Basic.InputBase where 
import Effect (Effect)
import Foreign (Foreign)
import Prelude
import Prim.Row (class Union)
import React.Basic.Events (EventHandler)
import React.Basic (JSX, ReactComponent, element)



foreign import data InputBasePropsInputRef :: Type

foreign import _inputBase :: forall a. ReactComponent a

foreign import data InputBaseComponentProps :: Type

  

-- | - `onChange`
-- |        `onChange`, `onKeyUp` + `onKeyDown` are applied to the inner `InputComponent`,
-- |        which by default is an input or textarea. Since these handlers differ from the
-- |        ones inherited by `React.HTMLAttributes<HTMLDivElement>` we need to omit them.
-- |        Note that  `blur` and `focus` event handler are applied to the outer `<div>`.
-- |        So these can just be inherited from the native `<div>`.

type InputBaseProps  = 
  ( autoComplete :: String
  ,  autoFocus :: Boolean
  ,  defaultValue :: Foreign
  ,  disabled :: Boolean
  ,  endAdornment :: JSX
  ,  error :: Boolean
  ,  fullWidth :: Boolean
  ,  id :: String
  ,  inputComponent :: JSX
  ,  inputProps :: InputBaseComponentProps
  ,  inputRef :: InputBasePropsInputRef
  ,  margin :: String
  ,  multiline :: Boolean
  ,  name :: String
  ,  onChange :: Foreign
  ,  onFilled :: (Effect Unit)
  ,  onKeyDown :: Foreign
  ,  onKeyUp :: Foreign
  ,  placeholder :: String
  ,  readOnly :: Boolean
  ,  renderPrefix :: EventHandler
  ,  required :: Boolean
  ,  rows :: String
  ,  rowsMax :: String
  ,  startAdornment :: JSX
  ,  type :: String
  ,  value :: Foreign
  ,  key :: String
  ,  children :: Array JSX
  )

inputBase
  :: forall attrs attrs_  
  . Union attrs attrs_ (InputBaseProps  )
  => Record attrs
  -> JSX
inputBase props = element _inputBase props
 

inputBase_ :: Array JSX -> JSX
inputBase_ children = inputBase { children }  

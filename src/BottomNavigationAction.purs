-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/BottomNavigationAction/BottomNavigationAction.d.ts
module MaterialUI.Basic.BottomNavigationAction where 
import Effect.Uncurried (EffectFn2)
import Foreign (Foreign)
import Prelude
import Prim.Row (class Union)
import React.Basic.Events (EventHandler)
import React.Basic (JSX, ReactComponent, element)



foreign import data BottomNavigationActionPropsIcon :: Type

foreign import _bottomNavigationAction :: forall a. ReactComponent a



type BottomNavigationActionProps  = 
  ( icon :: BottomNavigationActionPropsIcon
  ,  label :: JSX
  ,  onChange :: (EffectFn2 Foreign Foreign Unit)
  ,  onClick :: EventHandler
  ,  selected :: Boolean
  ,  showLabel :: Boolean
  ,  value :: Foreign
  ,  key :: String
  ,  children :: Array JSX
  )

bottomNavigationAction
  :: forall attrs attrs_  
  . Union attrs attrs_ (BottomNavigationActionProps  )
  => Record attrs
  -> JSX
bottomNavigationAction props = element _bottomNavigationAction props
 

bottomNavigationAction_ :: Array JSX -> JSX
bottomNavigationAction_ children = bottomNavigationAction { children }  

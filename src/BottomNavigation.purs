-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/BottomNavigation/BottomNavigation.d.ts
module MaterialUI.Basic.BottomNavigation where 
import Effect.Uncurried (EffectFn2)
import Foreign (Foreign)
import Prelude
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)





foreign import _bottomNavigation :: forall a. ReactComponent a



type BottomNavigationProps  = 
  ( component :: JSX
  ,  onChange :: (EffectFn2 Foreign Foreign Unit)
  ,  showLabels :: Boolean
  ,  value :: Foreign
  ,  key :: String
  ,  children :: Array JSX
  )

bottomNavigation
  :: forall attrs attrs_  
  . Union attrs attrs_ (BottomNavigationProps  )
  => Record attrs
  -> JSX
bottomNavigation props = element _bottomNavigation props
 

bottomNavigation_ :: Array JSX -> JSX
bottomNavigation_ children = bottomNavigation { children }  

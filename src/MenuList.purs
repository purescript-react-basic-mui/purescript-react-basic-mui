-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/MenuList/MenuList.d.ts
module MaterialUI.Basic.MenuList where 
import Prim.Row (class Union)
import React.Basic.Events (EventHandler)
import React.Basic (JSX, ReactComponent, element)





foreign import _menuList :: forall a. ReactComponent a



type MenuListProps  = 
  ( disableListWrap :: Boolean
  ,  onKeyDown :: EventHandler
  ,  key :: String
  ,  children :: Array JSX
  )

menuList
  :: forall attrs attrs_  
  . Union attrs attrs_ (MenuListProps  )
  => Record attrs
  -> JSX
menuList props = element _menuList props
 

menuList_ :: Array JSX -> JSX
menuList_ children = menuList { children }  

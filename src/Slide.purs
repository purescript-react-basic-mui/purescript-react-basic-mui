-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/Slide/Slide.d.ts
module MaterialUI.Basic.Slide where 
import Foreign (Foreign)
import Prim.Row (class Union)
import React.Basic.DOM.Internal (CSS)
import React.Basic (JSX, ReactComponent, Ref, element)

import MaterialUI.Basic.Styles.CreateMuiTheme (Theme)




foreign import _slide :: forall a. ReactComponent a


-- | - `appear`
-- |        Normally a component is not transitioned if it is shown when the
-- |        `<Transition>` component mounts. If you want to transition on the first
-- |        mount set  appear to true, and the component will transition in as soon
-- |        as the `<Transition>` mounts. Note: there are no specific "appear" states.
-- |        appear only adds an additional enter transition.
-- | - `enter`
-- |        Enable or disable enter transitions.
-- | - `exit`
-- |        Enable or disable exit transitions.

type SlideProps_optional  = 
  ( appear :: Boolean
  ,  enter :: Boolean
  ,  exit :: Boolean
  ,  ref :: (Ref Foreign)
  ,  style :: CSS
  ,  theme :: Theme
  ,  key :: String
  ,  children :: Array JSX
  )


-- | - `appear`
-- |        Normally a component is not transitioned if it is shown when the
-- |        `<Transition>` component mounts. If you want to transition on the first
-- |        mount set  appear to true, and the component will transition in as soon
-- |        as the `<Transition>` mounts. Note: there are no specific "appear" states.
-- |        appear only adds an additional enter transition.
-- | - `enter`
-- |        Enable or disable enter transitions.
-- | - `exit`
-- |        Enable or disable exit transitions.

type SlideProps_required   optional = 
  ( direction :: String
  | optional
  )

slide
  :: forall attrs attrs_  
  . Union attrs attrs_ (SlideProps_optional  )
  => Record ((SlideProps_required  ) attrs)
  -> JSX
slide props = element _slide props  

module MUI.Core.Grid where

import MUI.Core (JSS)
import MUI.Core.Grid.AlignContent (AlignContentProp)
import MUI.Core.Grid.AlignItems (AlignItemsProp)
import MUI.Core.Grid.Direction (DirectionProp)
import MUI.Core.Grid.GridCount (GridCountProp)
import MUI.Core.Grid.Justify (JustifyProp)
import MUI.Core.Grid.Spacing (SpacingProp)
import MUI.Core.Grid.Wrap (WrapProp)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_div)
import Unsafe.Coerce (unsafeCoerce)

type GridPropsOptions componentProps = 
  ( alignContent :: AlignContentProp
  , alignItems :: AlignItemsProp
  , children :: (Array JSX)
  , classes :: GridClassKey
  , component :: ReactComponent { | componentProps }
  , container :: Boolean
  , direction :: DirectionProp
  , item :: Boolean
  , justify :: JustifyProp
  , lg :: GridCountProp
  , md :: GridCountProp
  , sm :: GridCountProp
  , spacing :: SpacingProp
  , wrap :: WrapProp
  , xl :: GridCountProp
  , xs :: GridCountProp
  , zeroMinWidth :: Boolean
  | componentProps
  )

foreign import data GridProps :: Type



type GridClassKeyGenericOptions a =
  ( root :: a 
  , container :: a 
  , item :: a 
  , zeroMinWidth :: a 
  , "direction-xs-column" :: a 
  , "direction-xs-column-reverse" :: a 
  , "direction-xs-row-reverse" :: a 
  , "wrap-xs-nowrap" :: a 
  , "wrap-xs-wrap-reverse" :: a 
  , "align-items-xs-center" :: a 
  , "align-items-xs-flex-start" :: a 
  , "align-items-xs-flex-end" :: a 
  , "align-items-xs-baseline" :: a 
  , "align-content-xs-center" :: a 
  , "align-content-xs-flex-start" :: a 
  , "align-content-xs-flex-end" :: a 
  , "align-content-xs-space-between" :: a 
  , "align-content-xs-space-around" :: a 
  , "justify-xs-center" :: a 
  , "justify-xs-flex-end" :: a 
  , "justify-xs-space-between" :: a 
  , "justify-xs-space-around" :: a 
  , "justify-xs-space-evenly" :: a 
  , "spacing-xs-1" :: a 
  , "spacing-xs-2" :: a 
  , "spacing-xs-3" :: a 
  , "spacing-xs-4" :: a 
  , "spacing-xs-5" :: a 
  , "spacing-xs-6" :: a 
  , "spacing-xs-7" :: a 
  , "spacing-xs-8" :: a 
  , "spacing-xs-9" :: a 
  , "spacing-xs-10" :: a 
  , "grid-xs-auto" :: a 
  , "grid-xs-true" :: a 
  , "grid-xs-1" :: a 
  , "grid-xs-2" :: a 
  , "grid-xs-3" :: a 
  , "grid-xs-4" :: a 
  , "grid-xs-5" :: a 
  , "grid-xs-6" :: a 
  , "grid-xs-7" :: a 
  , "grid-xs-8" :: a 
  , "grid-xs-9" :: a 
  , "grid-xs-10" :: a 
  , "grid-xs-11" :: a 
  , "grid-xs-12" :: a 
  )
type GridClassKeyOptions = GridClassKeyGenericOptions String
type GridClassKeyJSSOptions = GridClassKeyGenericOptions JSS
foreign import data GridClassKey :: Type
foreign import data GridClassKeyJSS :: Type

gridClassKey :: ∀  given required
  .  Union given required (GridClassKeyOptions )
  => Record given
  -> GridClassKey
gridClassKey = unsafeCoerce

gridClassKeyJSS :: ∀  given required
  .  Union given required (GridClassKeyJSSOptions )
  => Record given
  -> GridClassKeyJSS
gridClassKeyJSS = unsafeCoerce

grid :: ∀  given required
  .  Union given required (GridPropsOptions Props_div )
  => Record given
  -> JSX
grid = element _Grid

grid_component :: ∀ componentProps given required
  .  Union given required (GridPropsOptions componentProps)
  => Record given
  -> JSX
grid_component = element _Grid

foreign import _Grid :: ∀ a. ReactComponent a
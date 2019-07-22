module MUI.Core.Grid.Justify where

import Unsafe.Coerce (unsafeCoerce)

foreign import data JustifyProp :: Type

data Justify = FlexStart | Center | FlexEnd | SpaceBetween | SpaceAround | SpaceEvenly

justify :: Justify -> JustifyProp
justify FlexStart = unsafeCoerce "flex-start"
justify Center = unsafeCoerce "center"
justify FlexEnd = unsafeCoerce "flex-end"
justify SpaceBetween = unsafeCoerce "space-between"
justify SpaceAround = unsafeCoerce "space-around"
justify SpaceEvenly = unsafeCoerce "space-evenly"


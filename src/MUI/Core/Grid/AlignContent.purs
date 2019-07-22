module MUI.Core.Grid.AlignContent where

import Unsafe.Coerce (unsafeCoerce)

foreign import data AlignContentProp :: Type

data AlignContent = Stretch | Center | FlexStart | FlexEnd | SpaceBetween | SpaceAround

alignContent :: AlignContent -> AlignContentProp
alignContent Stretch = unsafeCoerce "stretch"
alignContent Center = unsafeCoerce "center"
alignContent FlexStart = unsafeCoerce "flex-start"
alignContent FlexEnd = unsafeCoerce "flex-end"
alignContent SpaceBetween = unsafeCoerce "space-between"
alignContent SpaceAround = unsafeCoerce "space-around"

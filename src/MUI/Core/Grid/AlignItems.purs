module MUI.Core.Grid.AlignItems where

import Unsafe.Coerce (unsafeCoerce)

foreign import data AlignItemsProp :: Type

data AlignItems = FlexStart | Center | FlexEnd | Stretch | Baseline

alignItems :: AlignItems -> AlignItemsProp
alignItems FlexStart = unsafeCoerce "flex-start"
alignItems Center = unsafeCoerce "center"
alignItems FlexEnd = unsafeCoerce "flex-end"
alignItems Stretch = unsafeCoerce "stretch"
alignItems Baseline = unsafeCoerce "baseline"
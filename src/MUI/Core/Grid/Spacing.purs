module MUI.Core.Grid.Spacing where

import Unsafe.Coerce (unsafeCoerce)

foreign import data SpacingProp :: Type
data Spacing = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten

spacing :: Spacing -> SpacingProp
spacing Zero = unsafeCoerce 0
spacing One = unsafeCoerce 1
spacing Two = unsafeCoerce 2
spacing Three = unsafeCoerce 3
spacing Four = unsafeCoerce 4
spacing Five = unsafeCoerce 5
spacing Six = unsafeCoerce 6
spacing Seven = unsafeCoerce 7
spacing Eight = unsafeCoerce 8
spacing Nine = unsafeCoerce 9
spacing Ten = unsafeCoerce 10

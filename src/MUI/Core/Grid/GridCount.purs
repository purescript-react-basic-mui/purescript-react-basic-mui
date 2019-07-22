module MUI.Core.Grid.GridCount where

import Unsafe.Coerce (unsafeCoerce)

foreign import data GridCountProp :: Type
data GridCount = False | Auto | True | One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Eleven | Twelve
gridCount :: GridCount -> GridCountProp
gridCount False = unsafeCoerce false
gridCount Auto = unsafeCoerce "auto"
gridCount True = unsafeCoerce true
gridCount One = unsafeCoerce 1
gridCount Two = unsafeCoerce 2
gridCount Three = unsafeCoerce 3
gridCount Four = unsafeCoerce 4
gridCount Five = unsafeCoerce 5
gridCount Six = unsafeCoerce 6
gridCount Seven = unsafeCoerce 7
gridCount Eight = unsafeCoerce 8
gridCount Nine = unsafeCoerce 9
gridCount Ten = unsafeCoerce 10
gridCount Eleven = unsafeCoerce 11
gridCount Twelve = unsafeCoerce 12
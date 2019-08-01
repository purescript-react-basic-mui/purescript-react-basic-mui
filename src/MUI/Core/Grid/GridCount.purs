module MUI.Core.Grid.GridCount where

import Prelude

import Unsafe.Coerce (unsafeCoerce)

foreign import data GridCountProp :: Type
foreign import _eqGridCountProp :: GridCountProp -> GridCountProp -> Boolean
foreign import _ordGridCountProp :: GridCountProp -> GridCountProp -> Int
instance eqGridCountProp :: Eq GridCountProp where eq left right = _eqGridCountProp left right
instance ordGridCountProp :: Ord GridCountProp where compare left right = compare (_ordGridCountProp left right) (_ordGridCountProp right left)

boolean :: Boolean -> GridCountProp
boolean value = unsafeCoerce value

auto :: GridCountProp
auto = unsafeCoerce "auto"

one :: GridCountProp
one = unsafeCoerce 1.0

two :: GridCountProp
two = unsafeCoerce 2.0

three :: GridCountProp
three = unsafeCoerce 3.0

four :: GridCountProp
four = unsafeCoerce 4.0

five :: GridCountProp
five = unsafeCoerce 5.0

six :: GridCountProp
six = unsafeCoerce 6.0

seven :: GridCountProp
seven = unsafeCoerce 7.0

eight :: GridCountProp
eight = unsafeCoerce 8.0

nine :: GridCountProp
nine = unsafeCoerce 9.0

ten :: GridCountProp
ten = unsafeCoerce 10.0

eleven :: GridCountProp
eleven = unsafeCoerce 11.0

twelve :: GridCountProp
twelve = unsafeCoerce 12.0
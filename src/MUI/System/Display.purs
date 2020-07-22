module MUI.System.Display where

import Prim.Row (class Union) as Row
import Unsafe.Coerce (unsafeCoerce)

foreign import data DisplayValue ∷ Type

block ∷ DisplayValue
block = unsafeCoerce "block"

flex ∷ DisplayValue
flex = unsafeCoerce "flex"

inline ∷ DisplayValue
inline = unsafeCoerce "inline"

inlineBlock ∷ DisplayValue
inlineBlock = unsafeCoerce "inline-block"

none ∷ DisplayValue
none = unsafeCoerce "none"

foreign import data Display ∷ Type

display ∷ DisplayValue → Display
display = unsafeCoerce

type BreakPoints
  = ( lg :: DisplayValue
    , md :: DisplayValue
    , sm :: DisplayValue
    , xl :: DisplayValue
    , xs :: DisplayValue
    )

hiding ∷ ∀ given missing. Row.Union given missing BreakPoints ⇒ { | given } → Display
hiding = unsafeCoerce

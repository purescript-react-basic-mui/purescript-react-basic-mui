module MUI.Core.Styles.Shape where

import Unsafe.Coerce (unsafeCoerce)

type Shape
  = { borderRadius :: Number }

foreign import data ShapeOptions :: Type

shapeOptions :: Shape -> ShapeOptions
shapeOptions = unsafeCoerce

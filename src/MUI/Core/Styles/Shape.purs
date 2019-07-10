module MUI.Core.Styles.Shape where

import Data.Maybe (Maybe(..))

type Shape = { borderRadius :: Number }
type ShapeOptions = { borderRadius :: Maybe Number }

shapeOptions :: ShapeOptions
shapeOptions = { borderRadius : Nothing }
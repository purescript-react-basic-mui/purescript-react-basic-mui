module MUI.SSR where

import Prelude
import Effect (Effect)
import React.Basic (JSX)

foreign import kind Sheet

foreign import data Collected ∷ Sheet

foreign import data ServerStyleSheets ∷ Type

newtype JSXRender
  = JSXRender String

newtype JSSRender
  = JSSRender String

-- | Lower level API. Usually you just use `render` which combines these steps.
-- | Given these two functions you can build up your final app document
-- | by composing these pieces - like suggested here:
-- | https://github.com/cssinjs/examples/blob/gh-pages/react-ssr/src/server.js
foreign import serverStyleSheetsImpl ∷ Effect ServerStyleSheets

foreign import collectImpl ∷ JSX → ServerStyleSheets → Effect JSXRender

foreign import renderStyleSheetsImpl ∷ ServerStyleSheets → Effect JSSRender

render ∷ JSX → Effect { html ∷ String, stylesheet ∷ String }
render jsx = do
  s ← serverStyleSheetsImpl
  JSXRender html ← collectImpl jsx s
  JSSRender stylesheet ← renderStyleSheetsImpl s
  pure { html, stylesheet }

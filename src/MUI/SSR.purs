module MUI.SSR where

import Effect (Effect)
import React.Basic (JSX)

foreign import data ServerStyleSheets ∷ Type

newtype JSXRender
  = JSXRender String

newtype JSSRender
  = JSSRender String

foreign import serverStyleSheets ∷ Effect ServerStyleSheets

-- | Given these two functions you can build up your final app document
-- | by composing these pieces - like suggested here:
-- | https://github.com/cssinjs/examples/blob/gh-pages/react-ssr/src/server.js
foreign import collect ∷ JSX → ServerStyleSheets → Effect JSXRender

foreign import renderStyleSheet ∷ ServerStyleSheets → Effect JSSRender

-- | You can use these if you don't have styled components etc.
foreign import renderJSX ∷ JSX → JSXRender

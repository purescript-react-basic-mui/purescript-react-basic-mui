module MUI.Core.Styles.WithStyles where

import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import MUI.Core (JSS)
import MUI.Core.Styles.Types (Theme)
import React.Basic (ReactComponent)

foreign import withStylesImpl ::
  ∀ props.
  (Theme → JSS) → EffectFn1 (ReactComponent props) (ReactComponent props)

-- | Unsafe function which accepts any component and any JSS.
-- | It is used internally in well typed styling versions.
withStyles ∷ ∀ props. (Theme → JSS) → (ReactComponent props) → Effect (ReactComponent props)
withStyles t = runEffectFn1 (withStylesImpl t)

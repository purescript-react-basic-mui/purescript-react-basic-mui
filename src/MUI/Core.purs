module MUI.Core where

import Prelude
import Foreign (Foreign)
import Foreign.Object (Object)
import Foreign.Object as Object
import Prim.Row (class Nub)
import Record.Unsafe (unsafeSet)
import Unsafe.Coerce (unsafeCoerce)

type Color
  = { "50" :: String
    , "100" :: String
    , "200" :: String
    , "300" :: String
    , "400" :: String
    , "500" :: String
    , "600" :: String
    , "700" :: String
    , "800" :: String
    , "900" :: String
    , "A100" :: String
    , "A200" :: String
    , "A400" :: String
    , "A700" :: String
    }

jss :: forall r. { | r } -> JSS
jss = unsafeCoerce

foreign import data JSS :: Type

instance semigroupJSS :: Semigroup JSS where
  append jss1 jss2 =
    let
      (jss1Obj :: Object Foreign) = unsafeCoerce jss1

      (jss2Obj :: Object Foreign) = unsafeCoerce jss2
    in
      unsafeCoerce $ Object.union jss1Obj jss2Obj

instance monoidJSS :: Monoid JSS where
  mempty = jss {}

-- | Just a string underneath
newtype MediaQuery
  = MediaQuery String

mediaQuery ∷ MediaQuery → JSS → JSS
mediaQuery (MediaQuery mq) = let e = {} in \j → jss $ unsafeSet mq j e

-- | We are not able to encode optional / required fields in a nice way
-- | by using _oneof_ or _undefined-is-not-a-problem_
-- | libs here. The compilation time for rows from _react-basic_
-- | encoded and transformed in this manner is just unbearable:
-- |
-- | https://discourse.purescript.org/t/rowlist-iteration-seems-to-be-relatively-slow/1492
-- |
-- | On the other hand handling composition of rows when they use
-- | split architecture (`RequiredProps` vs `OptionalProps`) seems to
-- | make things complicated:
-- |
-- | ``` purescript
-- | type BaseButtonPropsOptional r = ( baseOptField :: Number, ... | r)
-- | type BaseButtonPropsRequired r = ( baseReqField :: String | r)
-- | ```
-- |
-- | Now let's imagine that we want to inherit from this and `Props_button`:
-- |
-- | ``` purescript
-- | type ButtonPropsOptional r = ( buttonOptField :: Number, ... | BaseButtonPropsOptional r )
-- | type ButtonPropsRequired r = ( buttonReqField :: String, ... | BaseButtonPropsRequired r )
-- | ```
-- |
-- | And now let's try to write a signature which handles `Nub` and `Unions` in this scenario.
-- |
-- | `given` is a row which represents a row / record which we want to get from the user.
-- |
-- | ``` purescript
-- | button
-- |  :: forall given opts'
-- |  . Nub (ButtonPropsRequired ()) req
-- |  => Union req optionalsGiven given
-- | ```
-- |
-- | The above states that `req` (which are nubbed required fields)
-- | are for sure part of the expected row. `optionalGiven` are filled
-- | by the compiler and not really exactly known and important to us in this formulation.
-- |
-- | Now we can add optionals to the mix - we want them to be possible part of the row
-- | and nothing more should go there so we sum all the fields up and state
-- | that the expected value should be a subset of all fields.
-- |
-- | ``` purescript
-- | => Nub (ButtonPropsOptional (ButtonPropsRequired Props_button)) all
-- | => Union given optionalsMissing all
-- | => { | given } -> ReactComponent all
-- | ```
class Nub i o <= Nub' (i ∷ # Type) (o ∷ # Type) | i → o

instance nub' ∷ (Nub i o) ⇒ Nub' i o

class ComponentPropsRow opaque props | opaque → props

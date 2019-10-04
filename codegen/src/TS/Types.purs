module Codegen.TS.Types where

import Control.Monad.Except (ExceptT)
import Effect (Effect)

type Errors = Array String

type M a = ExceptT (Array String) Effect a

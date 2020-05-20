module Codegen.TS.Types where

import Control.Monad.Except (ExceptT)
import Data.Map (Map)
import Effect (Effect)
import ReadDTS.Instantiation (Property, Type) as ReadDTS.Instantiation

type Errors
  = Array String

data InstantiationStrategy
  = TypeAlias
  | InterfaceInheritance

type InstanceProps
  = { fqn :: String -- Fully qualified name
    , props :: Map String (ReadDTS.Instantiation.Property ReadDTS.Instantiation.Type)
    }

type M a
  = ExceptT (Array String) Effect a

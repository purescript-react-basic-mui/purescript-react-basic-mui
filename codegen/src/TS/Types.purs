module Codegen.TS.Types where

import Control.Monad.Except (ExceptT)
import Data.Map (Map)
import Effect (Effect)
import ReadDTS.Instantiation (Property, Type) as Instantiation

type Errors = Array String

data InstantiationStrategy = TypeAlias | InterfaceInheritance

type InstanceProps =
  { fqn :: String
  , props :: Map String (Instantiation.Property Instantiation.Type)
  }

type M a = ExceptT (Array String) Effect a

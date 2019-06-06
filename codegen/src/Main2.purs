module Codegen.Main2 where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Generic.Rep as GR
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), isJust)
import Data.Nullable (Nullable)
import Data.String.Regex as String
import Data.String.Regex.Flags as RegexFlags
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import Foreign (Foreign)
import Foreign.Generic (class Decode, decode, defaultOptions, genericDecode)

data DeclarationSourceFile 
  = DeclarationSourceFile { fileName :: String, declarationModuleElements :: Array DeclarationModuleElements }

data DeclarationModuleElements
  = DeclarationElement DeclarationElements
  | ImportDeclaration
  | ImportEqualsDeclaration
  | ExportDeclaration
  | ExportDefaultDeclaration
  | ExportAssignment

data DeclarationElements
  = InterfaceDeclaration { name :: String, fullyQualifiedName :: String }
  | TypeAliasDeclaration { name :: String, aliasName :: Maybe String }
  | AmbientDeclaration

derive instance genericDeclarationSourceFile :: GR.Generic DeclarationSourceFile _
derive instance genericDeclarationModuleElements :: GR.Generic DeclarationModuleElements _
derive instance genericDeclarationElements :: GR.Generic DeclarationElements _

instance decodeDeclarationSourceFile :: Decode DeclarationSourceFile where decode = genericDecode defaultOptions
instance decodeDeclarationModule :: Decode DeclarationModuleElements where decode = genericDecode defaultOptions
instance decodeDeclarationElements :: Decode DeclarationElements where decode = genericDecode defaultOptions

instance showDeclarationSourceFile :: Show DeclarationSourceFile where show = genericShow
instance showDeclarationModule :: Show DeclarationModuleElements where show = genericShow 
instance showDeclarationElements :: Show DeclarationElements where show = genericShow



main :: Effect Unit
main = do
  sources       <- sourceFiles
  avatarRegex   <- liftEither $ String.regex "\\/Avatar.d.ts$" RegexFlags.noFlags
  cardRegex     <- liftEither $ String.regex "\\/Card.d.ts$" RegexFlags.noFlags
  avatar        <- liftMaybe "Couldn't find Avatar" $ Array.head $ Array.filter (\(DeclarationSourceFile { fileName }) -> isJust $ String.match avatarRegex fileName) sources
  card          <- liftMaybe "Couldn't find Card" $ Array.head $ Array.filter (\(DeclarationSourceFile { fileName }) -> isJust $ String.match cardRegex fileName) sources
  log $ show avatar
  log "\n\n"
  log $ show card


sourceFiles :: Effect (Array DeclarationSourceFile)
sourceFiles = do
  fs <- _sourceFiles
  traverse (liftEither <<< runExcept <<< decode) fs

liftEither :: ∀ e a. Show e => Either e a -> Effect a
liftEither = case _ of 
  Left e -> throw $ show e
  Right a -> pure a

liftMaybe :: ∀ a. String -> Maybe a -> Effect a
liftMaybe msg = case _ of 
  Nothing -> throw msg
  Just a -> pure a




foreign import _sourceFiles :: Effect (Array Foreign)
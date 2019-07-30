module Main where

import Prelude

import Codegen.Core.ExpansionPanel as ExpansionPanel
import Codegen.Model (Component, File(..), PropType(..))
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Foreign.Object (Object)
import Foreign.Object as Object

components :: Array Component
components = 
  [ ExpansionPanel.component
  ]

handleImports :: Object PropType -> String
handleImports obj = do
  let (array :: (Array (Tuple String (Array String)))) = Array.catMaybes $
        ((Object.toUnfoldable obj <#> snd) <#> extractImports) >>= identity
      (modules :: Object (Array String)) = Object.fromFoldable array
      (sorted :: (Array (Tuple String (Array String)))) = Object.toUnfoldable modules # (Array.sortWith fst)
  Array.intercalate "\n" 
      $ map (\(Tuple moduleName types) -> Array.fold ["import ", moduleName, " (", (Array.intercalate ", " types), ")" ]) 
      sorted

extractImports :: PropType -> Array (Maybe (Tuple String (Array String)))
extractImports (ArrayProp prop) = extractImports prop
extractImports (ImportProp moduleName typeName) = [ Just (Tuple moduleName [ typeName ]) ]
extractImports (ReactComponent moduleName typeName) = [ Just (Tuple moduleName [ typeName ]) ]
extractImports (PropList propType1 propType2) = (extractImports propType1) <> (extractImports propType2)
extractImports _ = []

genModuleName :: File -> String
genModuleName f = "module MUI." <> go f 
  where
    go :: File -> String
    go (Directory str file) = str <> "." <> go file
    go (File name) = name <> " where"

genPureScript :: Component -> String
genPureScript component = do
  let moduleName = genModuleName component.js
      imports = handleImports component.props
  Array.intercalate "\n\n" [ moduleName, imports ]

handleComponent :: Component -> Aff Unit
handleComponent component = do
  (log $ genPureScript component) # liftEffect

main :: Effect Unit
main = launchAff_ 
  $ traverse handleComponent components
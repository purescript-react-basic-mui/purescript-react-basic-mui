module Main where

import Prelude

import Codegen (Codegen(..), codegen)
import Codegen (javaScriptFile, pureScriptFile, write) as Codegen
import Codegen.AST (Ident(..))
import Codegen.AST.Sugar.Type (app, constructor, record, row) as Type
import Codegen.Model (ModulePath(..), Component, arrayJSX, jsx, reactComponentApply)
import Codegen.TS.MUI (ComponentName)
import Codegen.TS.MUI (componentProps) as TS.MUI
import Control.Alt ((<|>))
import Control.Monad.Except (runExceptT)
import Data.Array (null, sort) as Array
import Data.Array (filter)
import Data.Either (Either(..))
import Data.Foldable (for_, intercalate)
import Data.List (sort) as List
import Data.Map (filterWithKey, fromFoldable, keys, lookup, toUnfoldable) as Map
import Data.Map.Internal (keys) as Map.Internal
import Data.Maybe (Maybe(..))
import Data.Set (difference, member, unions) as Set
import Data.String (Pattern(..), split)
import Data.String (null) as String
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Matryoshka (cata)
import Node.Path (FilePath)
import Options.Applicative (Parser, ReadM, command, eitherReader, execParser, flag', fullDesc, help, helper, info, long, metavar, option, progDesc, short, showDefaultWith, strOption, subparser, value, (<**>))
import ReadDTS.Instantiation.Pretty (pprintTypeName)

components ∷ Array Component
components =
  let
    badge =
      let
        var = Ident "componentProps"
        component = reactComponentApply [ Type.record <<< Type.row mempty $ Just $ Left var ]
        baseProps = Map.fromFoldable
          [ Tuple "badgeContent" jsx
          , Tuple "children" arrayJSX
          , Tuple "component" component
          ]
        base = Type.row baseProps (Just (Left var))
      in
        { extraCode: Nothing
        , inherits: Nothing
        , name: "Badge"
        , modulePath: Path "MUI" $ Path "Core" $ Name "Badge"
        , propsType:
          { base: Just base
          , generate: ["anchorOrigin", "classes", "color", "invisible", "max", "showZero", "variant"]
          , vars: [ var ]
          }
        , tsc: { strictNullChecks: false }
        }
    appBar =
      let
        var = Ident "componentProps"
        baseProps = Map.fromFoldable [ Tuple "children" arrayJSX ]
        base = Type.row baseProps (Just (Left var))
      in
        { extraCode: Nothing
        , inherits: Just $ Type.app
            (Type.constructor "MUI.Core.Paper.PaperProps")
            [Type.constructor "React.Basic.DOM.Props_div"]
        , name: "AppBar"
        , modulePath: Path "MUI" (Path "Core" (Name "AppBar"))
        , propsType:
          { base: Just base
          , generate: ["classes", "color", "position"]
          , vars: [ var ]
          }
        , tsc: { strictNullChecks: false }
        }
  in
    [ appBar, badge ]


multiString :: Pattern -> ReadM (Array String)
multiString splitPattern = eitherReader \s ->
  let strArray = filter (not <<< String.null) $ split splitPattern s
  in
    if Array.null strArray
      then Left "got empty string as input"
      else Right strArray

commaSeparatedStringList :: Parser (Array String)
commaSeparatedStringList = option (multiString $ Pattern ",")
    ( long "skip-props"
   <> short 's'
   <> metavar "component1,component2,...,component3"
   <> help helpText
   <> value [ ]
   <> showDefaultWith (\array -> intercalate "," array)
    )
  where
    helpText = intercalate "."
      [ "A comma-separated list of component names "
      , "which props should be dropped from props list."
      , "This can be useful if you want to list only component own "
      , "properties or destil some inheritance etc."
      ]

showPropsOptions :: Parser Options
showPropsOptions = map ShowPropsCommand $ { component: _, skip: _ }
  <$> strOption (long "component" <> short 'c')
  <*> commaSeparatedStringList -- (long "skip-components" <> short 's')

data GenOutput
  = Directory FilePath
  | Stdout

genOutput :: Parser GenOutput
genOutput = directory <|> stdout
  where
    directory = map Directory $ strOption $
      long "directory"
      <> short 'd'
      <> metavar "DIRECTORY"
      <> help "Write output components to directory"
    stdout = flag' Stdout $
      long "stdout"
      <> short 's'
      <> help "Print component to stdout"

genOptions :: Parser Options
genOptions = map Generate $ { component: _, output: _ }
  <$> (Just <$> (option componentRead (long "component" <> short 'c')) <|> pure Nothing)
  <*> ((Just <$> genOutput) <|> pure Nothing)
  where
    componentRead :: ReadM Component
    componentRead = eitherReader $ \s -> case s `Map.lookup` components' of
      Just c → pure c
      otherwise → Left $ intercalate " "
        [ "Unkown component name"
        , show s
        ,". Please use one of:"
        , intercalate ", " (map show <<< List.sort <<< Map.Internal.keys $ components') <> "."
        ]
      where
        components' = Map.fromFoldable $ map (\c → Tuple c.name c) components

data Options
  = ShowPropsCommand
    { component ∷ ComponentName
    , skip ∷ Array ComponentName
    }
  | Generate
    { component ∷ Maybe Component
    , output ∷ Maybe GenOutput
    }


options ∷ Parser Options
options = subparser $
  command "codegen" (info genOptions (progDesc "Codegen all or a given module"))
  <> command "show-props" (info showPropsOptions (progDesc "Show information about props for a given component"))

main :: Effect Unit
main = do
  opts ← execParser (info (options <**> helper) fullDesc)
  let
    -- | Should I bring back multimodule handling logic?
    -- | For sure we want to keep track of the naming collisions
    -- | during codegen so maye we can just require that
    -- | we would have an unique naming strategy.
    writeModules dir modulePath code = launchAff_ $ do
      let
        js = Codegen (Codegen.javaScriptFile modulePath) code.js
        ps = Codegen (Codegen.pureScriptFile modulePath) code.ps
      Codegen.write dir js
      Codegen.write dir ps
    codegenComponent component output = runExceptT (codegen component) >>= case _ of
        Right code → case output of
          Just Stdout → do
            log "\nPureScript:"
            log code.ps
            log "\nJavasScript:"
            log code.js
          Just (Directory d) → do
            writeModules d component.modulePath code
          Nothing → do
            writeModules "../src" component.modulePath code
        Left err → log $ "Codegen errors: " <> intercalate "\n" (map show err)

  case opts of
    ShowPropsCommand { component, skip } → do
      let
        getProps = do
          c ← TS.MUI.componentProps component
          s <- traverse TS.MUI.componentProps skip
          pure { component: c, skip: s }
      runExceptT getProps >>= case _ of
        Right { component: cProps, skip: s } → do
          let
            cKeys = Map.keys cProps
            sKeys = Set.unions (map Map.keys s)
            keys = cKeys `Set.difference` sKeys
            props = Array.sort
              $ map (\(Tuple k v) → k <> " : " <> cata pprintTypeName v.type)
              $ Map.toUnfoldable
              $ Map.filterWithKey (\k v → k `Set.member` keys) cProps
          log $ intercalate "\n" props
        Left err → log $ intercalate "\n" err
    Generate { component: Just component, output } →
      codegenComponent component output
    Generate { output } → for_ components \component →
      codegenComponent component output


{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "mui-codegen"
, dependencies =
  [ "arrays"
  , "effect"
  , "exceptions"
  , "foreign"
  , "foreign-object"
  , "functions"
  , "maybe"
  , "prelude"
  , "psci-support"
  , "react-basic"
  , "react-basic-classic"
  , "react-basic-dom"
  , "react-basic-hooks"
  , "record"
  , "simple-json"
  , "tuples"
  , "type-equality"
  , "typelevel-prelude"
  , "unsafe-coerce"
  , "unsafe-reference"
  , "web-dom"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "examples/**/*.purs" ]
}

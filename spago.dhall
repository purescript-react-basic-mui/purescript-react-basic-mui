{ sources = [ "src/**/*.purs", "test/**/*.purs" ]
, name = "react-basic-mui"
, dependencies =
  [ "aff"
  , "effect"
  , "foreign"
  , "foreign-object"
  , "functions"
  , "prelude"
  , "react-basic"
  , "react-basic-dom"
  , "react-basic-hooks"
  , "record"
  , "simple-json"
  , "spec"
  , "type-equality"
  , "typelevel-prelude"
  , "unsafe-coerce"
  , "unsafe-reference"
  ]
, packages = ./packages.dhall
}

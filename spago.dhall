{ sources = [ "src/**/*.purs", "test/**/*.purs" ]
, name = "react-basic-mui"
, dependencies =
  [ "debug"
  , "react-basic"
  , "react-basic-dom"
  , "react-basic-hooks"
  , "simple-json"
  , "spec"
  , "strings-extra"
  , "strings"
  , "typelevel-eval"
  , "typelevel-prelude"
  , "unsafe-reference"
  ]
, packages = ./packages.dhall
}

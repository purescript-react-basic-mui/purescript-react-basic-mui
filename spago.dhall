{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, name =
    "react-basic-mui"
, dependencies =
    [ "debug", "react-basic-hooks", "simple-json", "spec", "unsafe-reference", "typelevel-prelude" ]
, packages =
    ./packages.dhall
}

{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, name =
    "react-basic-mui"
, dependencies =
    [ "debug"
    , "literals"
    , "react-basic"
    , "react-basic-hooks"
    , "simple-json"
    , "spec"
    , "unsafe-reference"
    , "untagged-union"
    , "typelevel-prelude"
    , "typelevel-eval"
    ]
, packages =
    ./packages.dhall
}

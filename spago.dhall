{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, name =
    "react-basic-mui"
, dependencies =
    [ "debug"
    , "homogeneous"
    , "react-basic-hooks"
    , "simple-json"
    , "spec"
    , "unsafe-reference"
    , "typelevel-prelude"
    , "typelevel-eval"
    ]
, packages =
    ./packages.dhall
}

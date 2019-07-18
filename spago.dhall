{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, name =
    "react-basic-mui"
, dependencies =
    [ "debug"
    , "optparse"
    , "node-fs-aff"
    , "react-basic-hooks"
    , "simple-json"
    , "spec"
    , "unicode"
    ]
, packages =
    ./packages.dhall
}

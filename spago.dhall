{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, name =
    "react-basic-mui"
, dependencies =
    [ "debug", "react-basic-hooks", "simple-json", "spec", "subrecord", "unsafe-reference" ]
, packages =
    ./packages.dhall
}

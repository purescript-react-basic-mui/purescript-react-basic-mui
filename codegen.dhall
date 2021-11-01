{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "mui-codegen"
, dependencies =
    [ "console"
    , "debug"
    , "effect"
    , "filterable"
    , "freet"
    , "heterogeneous"
    , "moldy"
    , "node-fs-aff"
    , "node-fs-extra"
    , "optparse"
    , "pprint"
    , "psci-support"
    , "read-dts"
    , "record-extra"
    , "react-basic"
    , "simple-json"
    , "sized-vectors"
    , "strings-extra"
    , "strings"
    , "debug"
    ]
    # (./spago.dhall).dependencies
, packages =
    ./packages.dhall
, sources =
    [ "./codegen/**/*.purs" ]
}

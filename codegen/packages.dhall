let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.0-20190602/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.0-20190602/src/packages.dhall sha256:5da1578dd297709265715a92eda5f42989dce92e121fcc889cff669a3b997c3d

let overrides = {=}

let additions =
      { readts =
          mkPackage
          [ "node-fs"
          , "strings"
          , "exceptions"
          , "argonaut-codecs"
          , "debug"
          , "console"
          , "argonaut-core"
          , "prelude"
          ]
          "https://github.com/doolse/purescript-readts.git"
          "master"
      
      , generics-rep-optics =
          mkPackage
          [ "generics-rep"
          , "profunctor-lenses"
          , "typelevel-prelude"
          ]
          "https://github.com/LiamGoodacre/purescript-generics-rep-optics.git"
          "v1.1.0"
      }

in  upstream // overrides // additions

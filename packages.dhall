let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.0-20190626/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.6-20200309/packages.dhall sha256:9221987b4e7ea99ccd0efbe056f7bebc872cd92e0058efe5baa181d73359e7b3

let overrides =
      { react-basic =
              upstream.react-basic
          //  { version = "df358364ece1cd4760429afae0bcca0eb518eb03" }
      }

let additions = {=}

in  upstream // overrides // additions

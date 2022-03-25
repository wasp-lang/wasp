-- -optP-Wno-nonportable-include-path avoids warning caused by .../autogen/cabal_macros.h. on OSX.
-- -fwrite-ide-info and -hiedir=.hie tell GHC to write compile-time information about the code
-- to .hie directory. This information can then be used by other tools, e.g. stan (static analyzer).
{ version = "0.4.0.0"
, description =
    "Please see the README on GitHub at <https://github.com/wasp-lang/wasp/waspc#readme>"
, homepage = "https://github.com/wasp-lang/wasp/waspc#readme"
, bug-reports = "https://github.com/wasp-lang/wasp/issues"
, author = "Wasp Team"
, maintainer = "team@wasp-lang.dev"
, copyright = "Wasp, Inc."
, license = "MIT"
, license-file = "LICENSE"
, ghc-options =
  [ "-Wall"
  , "-optP-Wno-nonportable-include-path"
  , "-fwrite-ide-info -hiedir=.hie"
  ]
, default-extensions =
  [ "OverloadedStrings"
  , "ScopedTypeVariables"
  , "QuasiQuotes"
  , "TemplateHaskell"
  ]
}

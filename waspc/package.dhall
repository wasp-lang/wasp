-- TODO:
--  - Rename wasp-cli back to just wasp.
-- tasty-hspec 1.1.7 introduces breaking changes, which is why we have < 1.1.7 .
let exe-ghc-options = "-threaded -rtsopts -with-rtsopts=-N"

in      ./defaults.dhall
    /\  { name = "waspc"
        , extra-source-files = [ "README.md", "ChangeLog.md" ]
        , data-files =
          [ "Generator/templates/*"
          , "Generator/templates/**/*"
          , "Cli/bash-completion"
          , "Cli/templates/**/*"
          ]
        , data-dir = "data/"
        , github = "wasp-lang/wasp"
        , library =
          { source-dirs = "src"
          , dependencies =
            [ "base                  >= 4.7 && < 5"
            , "Glob                  ^>= 0.10.2"
            , "containers            ^>= 0.6.5"
            , "directory             ^>= 1.3.6 && < 1.4"
            , "dir-traverse          ^>= 0.2.3"
            , "filepath              ^>= 1.4.2"
            , "time                  ^>= 1.9.3"
            , "bytestring            ^>= 0.10.12"
            , "aeson                 ^>= 1.5.6"
            , "aeson-pretty          ^>= 0.8"
            , "text                  ^>= 1.2.4"
            , "template-haskell      ^>= 2.16.0"
            , "unordered-containers  ^>= 0.2.16"
            , "mtl                   ^>= 2.2.2"
            , "async                 ^>= 2.2.4"
            , "conduit               ^>= 1.3.4"
            , "exceptions            ^>= 0.10.4"
            , "split                 ^>= 0.2.3"
            , "conduit-extra         ^>= 1.3.5"
            , "process               ^>= 1.6.13"
            , "cryptohash-sha256     ^>= 0.11.102"
            , "mustache              ^>= 2.3.2"
            , "parsec                ^>= 3.1.14"
            , "path                  ^>= 0.9.2"
            , "path-io               ^>= 1.6.3"
            , "regex-tdfa            ^>= 1.3.1"
            , "strong-path           ^>= 1.1.2"
            , "unliftio              ^>= 0.2.20"
            , "utf8-string           ^>= 1.0.2"
            , "cryptonite            ^>= 0.29"
            , "fsnotify              ^>= 0.3.0"
            , "http-conduit          ^>= 2.3.8"
            , "uuid                  ^>= 1.3.15"
            , "array                 ^>= 0.5.4"
            ]
          , other-modules = "Paths_waspc"
          , verbatim.build-tool-depends
            =
              ''
                alex:alex
              , happy:happy
              ''
          }
        , internal-libraries.cli-lib
          =
          { source-dirs = "cli/src"
          , dependencies =
            [ "directory"
            , "base"
            , "filepath"
            , "time"
            , "aeson"
            , "mtl"
            , "async"
            , "exceptions"
            , "cryptonite"
            , "fsnotify"
            , "http-conduit"
            , "path"
            , "path-io"
            , "strong-path"
            , "utf8-string"
            , "uuid"
            , "waspc"
            ]
          }
        , executables.wasp-cli
          =
          { ghc-options = exe-ghc-options
          , source-dirs = "cli/exe"
          , main = "Main.hs"
          , dependencies = [ "base", "async", "waspc", "cli-lib" ]
          }
        , tests =
          { cli-test =
            { ghc-options = exe-ghc-options
            , main = "TastyDiscoverDriver.hs"
            , source-dirs = "cli/test"
            , dependencies =
              [ "base"
              , "waspc"
              , "cli-lib"
              , "QuickCheck            ^>= 2.14"
              , "tasty                 ^>= 1.4.2"
              , "tasty-hspec           >= 1.1 && < 1.1.7"
              , "tasty-quickcheck      ^>= 0.10"
              ]
            , verbatim.build-tool-depends = "tasty-discover:tasty-discover"
            }
          , e2e-test =
            { ghc-options = exe-ghc-options
            , source-dirs = "e2e-test"
            , main = "Main.hs"
            , dependencies =
              [ "aeson"
              , "directory"
              , "base"
              , "filepath"
              , "strong-path"
              , "text"
              , "mtl"
              , "bytestring"
              , "dir-traverse"
              , "aeson-pretty"
              , "process"
              , "tasty ^>= 1.4.2"
              , "tasty-hspec >= 1.1 && < 1.1.7"
              , "tasty-golden ^>= 2.3.5"
              ]
            , verbatim.build-tool-depends = "waspc:wasp-cli"
            }
          , waspc-test =
            { ghc-options = exe-ghc-options
            , source-dirs = "test"
            , main = "TastyDiscoverDriver.hs"
            , dependencies =
              [ "aeson"
              , "base"
              , "deepseq"
              , "filepath"
              , "mtl"
              , "parsec"
              , "path"
              , "split"
              , "strong-path"
              , "text"
              , "unordered-containers"
              , "waspc"
              , "QuickCheck            ^>= 2.14"
              , "tasty                 ^>= 1.4.2"
              , "tasty-hspec           >= 1.1 && < 1.1.7"
              , "tasty-quickcheck      ^>= 0.10"
              ]
            , verbatim.build-tool-depends = "tasty-discover:tasty-discover"
            }
          }
        , benchmarks.waspc-benchmarks
          =
          { ghc-options = exe-ghc-options
          , source-dirs = "benchmark"
          , main = "Main.hs"
          , dependencies = [ "base", "criterion ^>=1.5", "waspc" ]
          }
        }

module Wasp.Generator.Valid.TsConfig
  ( validateSrcTsConfig,
  )
where

import Control.Monad (void)
import Validation (validateAll)
import qualified Wasp.ExternalConfig.TsConfig as T
import Wasp.Generator.Monad (GeneratorError (GenericGeneratorError))
import Wasp.Generator.Valid.Validator (Validator, execValidator, failure, field, file)

validateSrcTsConfig :: T.TsConfig -> [GeneratorError]
validateSrcTsConfig =
  (fmap (GenericGeneratorError . show) <$>) . execValidator $
    file "tsconfig.json" validateTopLevel
  where
    -- References for understanding the required compiler options:
    --   - The comments in templates/sdk/wasp/tsconfig.json
    --   - https://www.typescriptlang.org/docs/handbook/modules/introduction.html
    --   - https://www.totaltypescript.com/tsconfig-cheat-sheet
    --   - https://www.typescriptlang.org/tsconfig/

    validateTopLevel =
      validateAll
        [ field "include" T.include (eqJust ["src"]),
          void . field "compilerOptions" T.compilerOptions validateCompilerOptions
        ]

    validateCompilerOptions =
      validateAll
        [ field "module" T._module (eqJust "esnext"),
          field "target" T.target (eqJust "esnext"),
          -- Since Wasp ends up bundling the user code, `bundler` is the most
          -- appropriate `moduleResolution` option.
          field "moduleResolution" T.moduleResolution (eqJust "bundler"),
          field "moduleDetection" T.moduleDetection (eqJust "force"),
          -- `isolatedModules` prevents users from using features that don't work
          -- with transpilers and would fail when Wasp bundles the code with rollup
          -- (e.g., const enums)
          field "isolatedModules" T.isolatedModules (eqJust True),
          field "jsx" T.jsx (eqJust "preserve"),
          field "strict" T.strict (eqJust True),
          field "esModuleInterop" T.esModuleInterop (eqJust True),
          field "lib" T.lib (eqJust ["dom", "dom.iterable", "esnext"]),
          field "allowJs" T.allowJs (eqJust True),
          -- Wasp internally uses TypeScript's project references to compile the
          -- code. Referenced projects may not disable emit, so we must specify an
          -- `outDir`.
          field "outDir" T.outDir (eqJust ".wasp/out/user"),
          -- The composite flag is required because Wasp uses project references
          -- (i.e., web app and server reference user code as a subproject)
          field "composite" T.composite (eqJust True),
          field "skipLibCheck" T.skipLibCheck (eqJust True)
        ]

eqJust :: (Eq a, Show a) => a -> Validator (Maybe a) ()
eqJust expected (Just actual) = eq expected actual
eqJust expected Nothing =
  failure $
    unwords
      [ "Missing value, expected",
        show expected ++ "."
      ]

eq :: (Eq a, Show a) => a -> Validator a ()
eq expected actual
  | actual == expected = pure ()
  | otherwise =
      failure $
        unwords
          [ "Expected",
            show expected,
            "but got",
            show actual ++ "."
          ]

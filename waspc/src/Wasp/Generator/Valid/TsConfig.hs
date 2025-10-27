module Wasp.Generator.Valid.TsConfig
  ( validateSrcTsConfig,
  )
where

import Control.Monad (void)
import Validation (validateAll)
import qualified Wasp.ExternalConfig.TsConfig as T
import Wasp.Generator.Monad (GeneratorError (GenericGeneratorError))
import Wasp.Generator.Valid.Validator (Validator, execValidator, failure, inField, inFile)

validateSrcTsConfig :: T.TsConfig -> [GeneratorError]
validateSrcTsConfig config =
  GenericGeneratorError . show
    <$> execValidator validateFile config
  where
    -- References for understanding the required compiler options:
    --   - The comments in templates/sdk/wasp/tsconfig.json
    --   - https://www.typescriptlang.org/docs/handbook/modules/introduction.html
    --   - https://www.totaltypescript.com/tsconfig-cheat-sheet
    --   - https://www.typescriptlang.org/tsconfig/

    validateFile =
      inFile "tsconfig.json" $
        validateAll
          [ inField "include" T.include (eqJust ["src"]),
            void . inField "compilerOptions" T.compilerOptions validateCompilerOptions
          ]

    validateCompilerOptions =
      validateAll
        [ inField "module" T._module (eqJust "esnext"),
          inField "target" T.target (eqJust "esnext"),
          -- Since Wasp ends up bundling the user code, `bundler` is the most
          -- appropriate `moduleResolution` option.
          inField "moduleResolution" T.moduleResolution (eqJust "bundler"),
          inField "moduleDetection" T.moduleDetection (eqJust "force"),
          -- `isolatedModules` prevents users from using features that don't work
          -- with transpilers and would fail when Wasp bundles the code with rollup
          -- (e.g., const enums)
          inField "isolatedModules" T.isolatedModules (eqJust True),
          inField "jsx" T.jsx (eqJust "preserve"),
          inField "strict" T.strict (eqJust True),
          inField "esModuleInterop" T.esModuleInterop (eqJust True),
          inField "lib" T.lib (eqJust ["dom", "dom.iterable", "esnext"]),
          inField "allowJs" T.allowJs (eqJust True),
          -- Wasp internally uses TypeScript's project references to compile the
          -- code. Referenced projects may not disable emit, so we must specify an
          -- `outDir`.
          inField "outDir" T.outDir (eqJust ".wasp/out/user"),
          -- The composite flag is required because Wasp uses project references
          -- (i.e., web app and server reference user code as a subproject)
          inField "composite" T.composite (eqJust True),
          inField "skipLibCheck" T.skipLibCheck (eqJust True)
        ]

eqJust :: (Eq a, Show a) => a -> Validator (Maybe a) ()
eqJust expected (Just actual)
  | actual == expected = pure ()
  | otherwise =
      failure $ "Expected " ++ show expected ++ " but got " ++ show actual ++ "."
eqJust expected Nothing =
  failure $ "Missing value, expected " ++ show expected ++ "."

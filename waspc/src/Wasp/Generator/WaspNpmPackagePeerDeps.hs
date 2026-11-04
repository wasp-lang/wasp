module Wasp.Generator.WaspNpmPackagePeerDeps
  ( waspNpmPackagePeerDeps,
  )
where

import qualified Wasp.ExternalConfig.Npm.Dependency as Npm.Dependency
import Wasp.Generator.DepVersions
  ( dotenvVersionRange,
    expressTypesVersionRange,
    expressVersionRange,
    kyVersionRange,
    prismaVersionRange,
    reactDomTypesVersionRange,
    reactDomVersionRange,
    reactQueryVersionRange,
    reactRouterVersionRange,
    reactTypesVersionRange,
    reactVersionRange,
    superjsonVersionRange,
    typescriptVersionRange,
    viteVersionRange,
  )

{-
The generated server and SDK import packages like express, react and
@prisma/client, but their package.json files don't list them. @wasp/wasp
(packages/wasp/package.json) lists them as peer dependencies instead, so
every Wasp project gets them by depending on @wasp/wasp.

They can't be regular dependencies of @wasp/wasp. When the project wants
another version of a regular dependency, npm nests @wasp/wasp's copy in
node_modules/@wasp/wasp/node_modules, where the generated code can't import it.

npm installs peer dependencies at the project root, where the generated code
finds them. Peer dependencies also ensure that the project can't install a
conflicting version, so the package.json validator doesn't need to check these
packages. A project that needs another version uses npm's `overrides` field.

Only the packages every app needs are peer dependencies. The packages that
only some apps need, like socket.io for WebSockets or pg-boss for jobs, stay
in the package.json files of the generated server and SDK for now.

"Generator.WaspNpmPackagePeerDepsTest" ensures that this list matches
packages/wasp/package.json.
-}

-- | The peer dependencies of @wasp/wasp.
waspNpmPackagePeerDeps :: [Npm.Dependency.Dependency]
waspNpmPackagePeerDeps = serverCoreDeps ++ sdkCoreDeps ++ userProjectDeps

-- | Packages the generated server needs to build and run in every app.
serverCoreDeps :: [Npm.Dependency.Dependency]
serverCoreDeps =
  Npm.Dependency.fromList
    [ ("@rollup/plugin-node-resolve", "^16.0.0"),
      ("@types/cors", "^2.8.5"),
      ("@types/express", show expressTypesVersionRange),
      ("@types/express-serve-static-core", show expressTypesVersionRange),
      ("cookie-parser", "~1.4.6"),
      ("cors", "^2.8.5"),
      ("dotenv", show dotenvVersionRange),
      ("express", show expressVersionRange),
      ("helmet", "^6.0.0"),
      ("morgan", "~1.12.1"),
      -- Used by `prepareProduction.ts` when building the Docker image.
      ("nf3", "0.4.2"),
      ("rollup", "^4.9.6"),
      ("rollup-plugin-esbuild", "^6.1.1"),
      ("superjson", show superjsonVersionRange),
      ("typescript", show typescriptVersionRange)
    ]

-- | Packages the generated SDK needs in every app.
sdkCoreDeps :: [Npm.Dependency.Dependency]
sdkCoreDeps =
  Npm.Dependency.fromList
    [ ("@prisma/client", show prismaVersionRange),
      ("@tanstack/react-query", show reactQueryVersionRange),
      ("@types/react", show reactTypesVersionRange),
      ("@types/react-dom", show reactDomTypesVersionRange),
      ("@vitejs/plugin-react", "^6.0.3"),
      ("ky", show kyVersionRange),
      ("react-hook-form", "^7.45.4")
    ]

userProjectDeps :: [Npm.Dependency.Dependency]
userProjectDeps =
  Npm.Dependency.fromList
    [ ("prisma", show prismaVersionRange),
      ("react", show reactVersionRange),
      ("react-dom", show reactDomVersionRange),
      ("react-router", show reactRouterVersionRange),
      ("vite", show viteVersionRange)
    ]

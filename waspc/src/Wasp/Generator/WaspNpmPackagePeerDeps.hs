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
@prisma/client without listing them. @wasp/wasp (packages/wasp/package.json)
lists them as peer dependencies instead:

- npm installs peers at the project root, where the generated code finds
  them. As regular dependencies of @wasp/wasp, npm would nest them in
  node_modules/@wasp/wasp/node_modules whenever the project wants another
  version.
- npm refuses a conflicting version of a peer, so the package.json validator
  doesn't check them. A project that needs another version uses `overrides`.

Only what every app needs is a peer. Packages only some apps need, like
socket.io, stay in the generated package.json files.

"Generator.WaspNpmPackagePeerDepsTest" keeps this list equal to
packages/wasp/package.json.
-}

-- | The peer dependencies of \@wasp/wasp.
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

-- | Packages the user's project runs or imports itself.
userProjectDeps :: [Npm.Dependency.Dependency]
userProjectDeps =
  Npm.Dependency.fromList
    [ ("prisma", show prismaVersionRange),
      ("react", show reactVersionRange),
      ("react-dom", show reactDomVersionRange),
      ("react-router", show reactRouterVersionRange),
      ("vite", show viteVersionRange)
    ]

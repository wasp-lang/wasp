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
    vitestVersionRange,
  )

{-
The generated server and SDK import packages like express, react and
@prisma/client, but their package.json files don't list them. @wasp/wasp
(packages/wasp/package.json) lists them as peer dependencies instead, so
every Wasp project gets them by depending on @wasp/wasp.

Regular dependencies of @wasp/wasp wouldn't work. If something else in the
project wants another version of a package, npm installs @wasp/wasp's copy in
node_modules/@wasp/wasp/node_modules, and the generated code in .wasp/out
can't import it from there.

npm installs a peer dependency at the project root and refuses to install a
conflicting version. So the generated code finds the package, and npm checks
its version, which is why the package.json validator skips these packages.
A project that needs another version uses npm's `overrides` field.

E.g., a project with `"react": "18.2.0"` in its dependencies fails
`npm install` with ERESOLVE, because @wasp/wasp asks for `react@^19.2.1`.

The generated packages list the dependencies that depend on the app
themselves, like socket.io for WebSockets or pg-boss for jobs.

"Generator.WaspNpmPackagePeerDepsTest" checks that this list matches
packages/wasp/package.json.
-}

-- | The peer dependencies of @wasp/wasp.
waspNpmPackagePeerDeps :: [Npm.Dependency.Dependency]
waspNpmPackagePeerDeps = serverCoreDeps ++ sdkCoreDeps ++ projectDeps

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
      ("@testing-library/jest-dom", "^6.9.1"),
      ("@testing-library/react", "^16.3.1"),
      ("@types/react", show reactTypesVersionRange),
      ("@types/react-dom", show reactDomTypesVersionRange),
      ("@vitejs/plugin-react", "^6.0.3"),
      -- TODO: Installing @wasp/wasp in a project without vitest fails with
      -- ERESOLVE. npm follows vite's optional peers down to
      -- @vitejs/devtools-vitest and resolves its optional `vitest@*` peer to
      -- vitest 5, which conflicts with @vitest/ui's exact vitest 4 peer. This
      -- goes away when Vitest moves to user-land (#5060) and the testing
      -- packages leave @wasp/wasp.
      ("@vitest/ui", "^4.1.9"),
      ("jsdom", "^27.4.0"),
      ("ky", show kyVersionRange),
      ("msw", "^2.12.7"),
      ("react-hook-form", "^7.45.4")
    ]

-- | Packages the user's code imports directly. The starters list them in
-- package.json too.
projectDeps :: [Npm.Dependency.Dependency]
projectDeps =
  Npm.Dependency.fromList
    [ ("prisma", show prismaVersionRange),
      ("react", show reactVersionRange),
      ("react-dom", show reactDomVersionRange),
      ("react-router", show reactRouterVersionRange),
      ("vite", show viteVersionRange),
      ("vitest", show vitestVersionRange)
    ]

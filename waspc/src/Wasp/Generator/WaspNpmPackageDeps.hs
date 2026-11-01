module Wasp.Generator.WaspNpmPackageDeps
  ( waspNpmPackageDeps,
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

-- | The packages @wasp/wasp (packages/wasp/package.json) declares as peer
-- dependencies for every Wasp project: everything the generated server and
-- SDK need no matter what the app uses, and the packages the project imports
-- itself. Generated code imports them without declaring them. They are peers
-- and not regular dependencies on purpose: npm installs a peer at the project
-- root and refuses a conflicting version there, while it would quietly nest a
-- regular dependency under @wasp/wasp when something else in the project wants
-- another version, where the generated code would not find it.
--
-- Since npm enforces their versions, the package.json validator doesn't check
-- them. A project that needs another version uses npm's `overrides`.
--
-- Not here are the dependencies that depend on the app (WebSockets, jobs,
-- email senders), which the generated packages still declare.
--
-- Keep in sync with packages/wasp/package.json, a unit test checks that.
waspNpmPackageDeps :: [Npm.Dependency.Dependency]
waspNpmPackageDeps = serverCoreDeps ++ sdkCoreDeps ++ projectDeps

-- | What the generated server used to declare itself.
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
      ("nodemon", "^2.0.19"),
      ("rollup", "^4.9.6"),
      ("rollup-plugin-esbuild", "^6.1.1"),
      ("superjson", show superjsonVersionRange),
      ("typescript", show typescriptVersionRange)
    ]

-- | What the generated SDK used to declare itself.
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
      -- TODO: Installing @wasp/wasp in a project that doesn't declare vitest
      -- (the npm-package-test smoke test) fails with ERESOLVE: npm resolves
      -- vite's optional `vitest@*` peer to vitest 5, which conflicts with this
      -- one. Goes away when Vitest moves to user-land (#5060) and the testing
      -- packages leave @wasp/wasp.
      ("@vitest/ui", "^4.1.9"),
      ("jsdom", "^27.4.0"),
      ("ky", show kyVersionRange),
      ("msw", "^2.12.7"),
      ("react-hook-form", "^7.45.4")
    ]

-- | What the project imports itself, so its package.json declares them too.
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

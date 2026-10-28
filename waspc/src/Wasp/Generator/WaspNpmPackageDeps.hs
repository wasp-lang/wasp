module Wasp.Generator.WaspNpmPackageDeps
  ( waspNpmPackageDeps,
    serverCoreDeps,
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
    reactQueryVersionRange,
    reactTypesVersionRange,
    superjsonVersionRange,
    typescriptVersionRange,
  )

-- | The dependencies that @wasp/wasp (packages/wasp/package.json) declares on
-- behalf of every Wasp project: everything the generated server and SDK need
-- regardless of what the app uses. Generated code imports them without
-- declaring them, relying on npm hoisting them to the project's node_modules.
--
-- Not here: dependencies that depend on the app (e.g. WebSockets, jobs, email
-- senders), which the generated packages still declare, and the ones the
-- user's package.json must declare itself (react, react-router, prisma, vite,
-- vitest), which the project owns.
--
-- Keep in sync with packages/wasp/package.json. A unit test checks that they
-- match.
waspNpmPackageDeps :: [Npm.Dependency.Dependency]
waspNpmPackageDeps = serverCoreDeps ++ sdkCoreDeps

-- | What the generated server used to declare itself. The package.json
-- validator checks that the user's package.json doesn't ask for a conflicting
-- version of these, which would make npm nest our copy where the generated
-- code can't see it.
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
      ("nodemon", "^2.0.19"),
      ("rollup", "^4.9.6"),
      ("rollup-plugin-esbuild", "^6.1.1"),
      ("superjson", show superjsonVersionRange),
      ("typescript", show typescriptVersionRange)
    ]

-- | What the generated SDK used to declare itself. Not validated against the
-- user's package.json, same as before (the starters themselves pin e.g.
-- react-hook-form to a newer range than this one).
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
      ("@vitest/ui", "^4.1.9"),
      ("jsdom", "^27.4.0"),
      ("ky", show kyVersionRange),
      ("msw", "^2.12.7"),
      ("react-hook-form", "^7.45.4")
    ]

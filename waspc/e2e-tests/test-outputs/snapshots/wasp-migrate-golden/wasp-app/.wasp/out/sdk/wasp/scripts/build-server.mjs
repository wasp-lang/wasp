import { createBuilder } from "vite"

// Bundles the Wasp server, i.e. builds the `server` Vite environment declared
// by the `waspServer()` plugin.
//
// We can't do this with the Vite CLI because it has no way of building a single
// environment: `vite build` builds the app (the `ssr` and `client`
// environments).
//
// This script must run from the Wasp project's directory (the Vite root),
// see the `bundle` script in the generated server's `package.json`.

// Keep in sync with `ENVIRONMENT_NAMES.SERVER` in `../vite/constants.ts`.
// We can't import it because this script runs from the package's sources, while
// the compiled SDK lives in `dist`.
const serverEnvironmentName = "server"

const builder = await createBuilder()
const serverEnvironment = builder.environments[serverEnvironmentName]

if (!serverEnvironment) {
  console.error(
    `Vite environment "${serverEnvironmentName}" not found. Make sure your vite.config.ts uses the \`waspServer()\` plugin from "wasp/server/vite".`,
  )
  process.exit(1)
}

await builder.build(serverEnvironment)

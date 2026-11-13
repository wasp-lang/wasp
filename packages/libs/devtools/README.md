# @wasp/lib-devtools

Wasp's panels for [Vite DevTools](https://devtools.vite.dev/), which the Wasp SDK's `wasp()` Vite plugin adds during `wasp start`. They show up when the project enables Vite DevTools with the `devtools` option in its `vite.config.ts` (`devtools: waspDevTools()` in new projects).

- `waspToolbar()`: Wasp's own toolbar in the app's page, which replaces the Vite DevTools floating dock (`waspDevTools()` hides it). It connects to the same DevTools server through `@vitejs/devtools-kit/client` and shows the panels in a panel of its own. Panels it can't render open in the full DevTools page instead.
- `waspStudio()`: runs `wasp studio` when the user opens the panel, and embeds it once it's up. It uses the Wasp executable that `wasp start` passes in the `WASP_EXE_PATH` env var, falling back to `wasp` from the `PATH`.
- `prismaStudio()`: starts `prisma studio` on a free port and embeds it. It runs Prisma from `.wasp/out/server`, like `wasp db studio`, and `wasp start` passes the dev database URL to the Vite process for it.
- `terminals()`: the Terminals panel, with the output of the processes the other panels start. Vite DevTools has its own, but `waspDevTools()` turns off its built-in panels.

## Development

To try a change, run `mise run build:libs` and restart `wasp start` in a Wasp app (with `mise run bust-libs-cache` if npm keeps the previous build).

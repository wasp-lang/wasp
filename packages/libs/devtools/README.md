# @wasp/lib-devtools

Wasp's panels for [Vite DevTools](https://devtools.vite.dev/), which the Wasp SDK's `wasp()` Vite plugin adds during `wasp start`. They show up when the project enables Vite DevTools with the `devtools` option in its `vite.config.ts` (which is enabled with `devtools: waspDevTools()`).

- `waspStudio()`: serves the Wasp Studio page (`packages/studio/client`, which the lib's build copies into `dist/studio`) from the Vite dev server, and embeds it. The page reads the app's spec from the server URL that `wasp start` passes to the Vite process.
- `prismaStudio()`: runs `wasp db studio` on a free port when the user opens the panel, and embeds it once it's up. It uses the Wasp executable that `wasp start` passes in the `WASP_EXE_PATH` env var, falling back to `wasp` from the `PATH`.

## Development

To try a change, run `mise run build:libs` and restart `wasp start` in a Wasp app (with `mise run bust-libs-cache` if npm keeps the previous build).

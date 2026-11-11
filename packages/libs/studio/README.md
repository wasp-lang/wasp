# @wasp/lib-studio

Wasp Studio shows how the parts of a Wasp app connect: pages, routes, operations, APIs, jobs, and entities. It is a [Devframe](https://devfra.me/) app that runs inside the app during `wasp start`, as a [Vite DevTools](https://devtools.vite.dev/) panel. The `waspStudio()` Vite plugin from `@wasp/lib-studio/node` registers the panel, and the Wasp SDK's `wasp()` plugin includes it. The panel shows up when the project enables Vite DevTools with the `devtools` option in its `vite.config.ts` (`devtools: waspDevTools()` in new projects).

It reads the JSON file Wasp writes to `.wasp/out/.wasp-studio-data.json` on every compile, and pushes it to the open panels when it changes.

## Development

The panel UI is the React app in `client/`, which the build bundles into `dist/client`. To try a change, run `mise run build:libs` and restart `wasp start` in a Wasp app (with `mise run bust-libs-cache` if npm keeps the previous build).

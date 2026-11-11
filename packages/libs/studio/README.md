# @wasp/lib-studio

Wasp Studio shows how the parts of a Wasp app connect: pages, routes, operations, APIs, jobs, and entities. It is a [Devframe](https://devfra.me/) app, so the same definition runs in two places:

- Inside the app during `wasp start`, as a [Vite DevTools](https://devtools.vite.dev/) panel. The `waspStudio()` Vite plugin from `@wasp/lib-studio/node` adds Vite DevTools to the dev server with Studio as one of its panels, and the Wasp SDK's `wasp()` plugin includes it.
- On its own with `wasp studio`, which runs `dist/standalone.mjs` on port 4000.

Both read the JSON file Wasp writes to `.wasp/out/.wasp-studio-data.json` on every compile, and push it to the open panels when it changes.

## Development

The panel UI is the React app in `client/`, which the build bundles into `dist/client`. To try a change, rebuild and run Studio against a compiled Wasp app:

```bash
pnpm run build
node dist/standalone.mjs --data-file ../../../examples/kitchen-sink/.wasp/out/.wasp-studio-data.json
```

Then open http://localhost:4000.

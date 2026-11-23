# Wasp Studio

Wasp Studio shows a Wasp app as a graph of its pages, routes, operations, APIs, jobs, and entities.

It has two components:

- the client, a page that reads the app's spec from `/__wasp/spec`, in the same JSON that `wasp show spec --json` prints.
- the server, which `wasp studio` runs on a free port (`4000` if it can). It serves the client, and the spec from the data file that `wasp studio` writes (`.wasp/out/.wasp-studio-data.json`).

The Wasp Studio panel in Vite DevTools (in `@wasp/lib-devtools`) serves the same client from the Vite dev server instead. It passes the client a `serverUrl` query parameter, so the client reads the spec from the server that `wasp start` runs, which has the same route in development.

### Server

Run `wasp studio` once in a Wasp app to write its data file. Then build the server and run it with that file:

```bash
pnpm run build
pnpm run dev --data-file path/to/app/.wasp/out/.wasp-studio-data.json --port 4000
```

### Client

Run `wasp start` in a Wasp app, and then run the client in development mode:

```bash
cd client
pnpm run dev
```

Then open the URL Vite prints, with `?serverUrl=http://localhost:3001` so it reads the spec from the app's server.

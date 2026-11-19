# Wasp Studio

Wasp Studio shows a Wasp app as a graph of its pages, routes, operations, APIs, jobs, and entities.

It has two components:

- the client, a page that reads the app's spec from the server that `wasp start` runs, at `/__wasp/spec`. The server only has this route in development.
- the server, which `wasp studio` runs to serve the client on `http://localhost:4000`.

To develop the studio, run `wasp start` in a Wasp app, and then run the client in development mode:

```bash
cd client
pnpm install
pnpm run dev
```

Then open `http://localhost:4000` in your browser. To try the server, run `pnpm run build` and then `pnpm run start`.

# Wasp Studio

Wasp Studio is a [devframe](https://devfra.me/) devtool that `wasp studio` runs.

- `src/index.ts`: the node side. It serves the client and keeps the app data in devframe shared state, updating it when the data file changes.
- `src/client/`: the React client, built into `dist/client`.
- `src/shared/`: code both sides use.

## Development

Wasp CLI generates the `.wasp/out/.wasp-studio-data.json` file when you run `wasp studio`. Run it once in a Wasp app to get the file.

Build the client in watch mode:

```bash
pnpm run dev:client
```

In another terminal, run the server for that data file:

```bash
pnpm run dev:server --data-file <path_to_data_file>
```

Then open the link the server prints. It signs your browser in, and the browser stays signed in after that.

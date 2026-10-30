# Wasp Studio

Wasp Studio has two components:

- the server
- the client

To develop the studio, you need to run both of them. First, run the server, then the client.

### Server

Install dependencies:

```bash
pnpm install
```

Running the server for some data file:

```bash
pnpm run dev -- -d <path_to_data_file>
```

For example, running the server with the data file from the `examples` directory:

```bash
pnpm run dev -- -d ../../examples/todoApp/.wasp/out/.wasp-studio-data.json
```

Wasp CLI generates the `.wasp-studio-data.json` file when the `wasp studio` command is run.
You can run the `wasp studio` once to get the file generated, and then run the studio in development mode.

### Client

Install dependencies:

```bash
cd client
pnpm install
```

Running the client:

```bash
pnpm run dev
```

Then open `http://localhost:5173` in your browser.
